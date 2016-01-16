(ns sicp.part2.4
  (:require [sicp.part1 :refer [square]])
  (:require [sicp.part2 :refer [eq? pair? variable?
                                same-variable? third
                                make-sum make-product
                                sum? product?
                                addend augend
                                multiplier multiplicand
                                exponentiation?
                                exponent base
                                make-exponentiation]]))

;; -------------------------------------------------
;; 2.4.3 Data-directed Programming and Additivitiy
;; -------------------------------------------------


;; NB in SICP in this chapter they assume two procedures 'put' and 'get' which add and read
;; from an 'operation-and-type' table. These functions are kind of fictional at the moment
;; so I will instead create a hacky implementation of them using a map in clojure.

;; Here I represent the data as a vector: [x y :rectangular] or [r a :polar]

(def operation-table (atom {}))

(defn put-fn
  "Installs the item in the table, indexed by the op and the type"
  [op type item]
  (swap! operation-table conj [[op type] item]))

(defn get-fn
  "Retrieves the item in the table, indexed by the op and the type"
  [op type]
  (@operation-table [op type]))

(defn attach-tag [tag item]
  (conj item tag))

(defn type-tag [item]
  (last item))

(defn contents [item]
  (pop item))

(defn install-rectangular-package
  []
  (letfn [
          (real-part [z]
            (first z))
          (imag-part [z]
            (second z))
          (make-from-real-imag [x y]
            [x y])
          (magnitute [z]
            (Math/sqrt (+ (square (real-part z))
                          (square (imag-part z)))))
          (angle [z]
                 (Math/atan (/ (imag-part z)
                               (real-part z))))
          (make-from-mag-ang [r a]
            ([(* r (Math/cos a)) (* r (Math/sin a))]))
          ;; interface to the rest of the system
          (tag [x] (attach-tag :rectangular x))]

    (put-fn 'real-part [:rectangular] real-part)
    (put-fn 'imag-part [:rectangular] imag-part)
    (put-fn 'magnitute [:rectangular] magnitute)
    (put-fn 'angle [:rectangular] angle)
    (put-fn 'make-from-real-imag :rectangular
            (fn [x y] (tag (make-from-real-imag x y))))
    (put-fn 'make-from-mag-ang :rectangular
            (fn [r a] (tag (make-from-mag-ang r a))))
    :done))

(defn install-polar-package
  []
  (letfn [(magnitute [z] (first z))
          (angle [z] (second z))
          (make-from-mag-ang [r a] [r a])
          (real-part [z]
            (* (magnitute z) (Math/cos (angle z))))
          (imag-part [z]
            (* (magnitute z) (Math/sin (angle z))))
          (make-from-real-imag [x y]
            (vector (Math/sqrt (+ (square x) (square y)))
                    (Math/atan (/ y x))))
          ;; interface to the rest of the system
          (tag [x] (attach-tag :polar x))]
    (put-fn 'real-part [:polar] real-part)
    (put-fn 'imag-part [:polar] imag-part)
    (put-fn 'magnitute [:polar] magnitute)
    (put-fn 'angle [:polar] angle)
    (put-fn 'make-from-real-imag :polar
            (fn [x y] (tag (make-from-real-imag x y))))
    (put-fn 'make-from-mag-ang :polar
            (fn [r a] (tag (make-from-mag-ang r a))))
    :done))


(defn apply-generic
  "Look in the operation table under the name op and the types of the
  arguments and apply the resulting procedure if it is present."
  [op & args]
  (let [type-tags (map type-tag args)
        proc (get-fn op type-tags)]
    (if proc
      (apply proc (map contents args))
      (throw (Exception. (str "No method for these types -- APPLY GENERIC"
                              (list op type-tags)))))))

(install-rectangular-package)
(install-polar-package)

(defn real-part
  [z]
  (apply-generic 'real-part z))

(defn imag-part
  [z]
  (apply-generic 'imag-part z))

(defn magnitute
  [z]
  (apply-generic 'magnitute z))

(defn angle
  [z]
  (apply-generic 'angle z))

(defn make-from-real-imag
  [x y]
  ((get-fn 'make-from-real-imag :rectangular) x y))

(defn make-from-mag-ang
  [r a]
  ((get-fn 'make-from-mag-ang :polar) r a))


(prn "---------------")
(prn "Exercise 2.73")
(prn "---------------")

;; (get <op> <type>) looks up the <op>, <type> entry in the table and
;; returns the item found there. If no item is found, get returns false.

;; (put <op> <type> <item>) installs the <item> in the table, indexed
;; by the <op> and the <type>

(print "- Part a.")
(print "A generic version of deriv is retrieved from the generic function
     table using the operator as the dispatch type.")
(print "number? and variable? can't be assimilated into the data-directed
      approach because they hinge upon the type of the value rather than
      the value of the symbol. There is no way to express both the operator
      value and the type of the value as the same question to reference the
      generic function table.")

(defn operator [exp] (first exp))

(defn deriv
  [exp var]
  (cond
   (number? exp) 0
   (variable? exp) (if (same-variable? exp var) 1 0)
   :else ((get-fn 'deriv (operator exp)) exp var)))

(defn install-sum-deriv
  []
  (letfn [(deriv-sum [exp var]
            (if (sum? exp)
              (make-sum (deriv (addend exp) var)
                        (deriv (augend exp) var))))]
    (put-fn 'deriv '+ deriv-sum)))

(defn install-product-deriv
  []
  (letfn [(deriv-product [exp var]
            (if (product? exp)
              (make-sum
               (make-product (multiplier exp)
                             (deriv (multiplicand exp) var))
               (make-product (deriv (multiplier exp) var)
                             (multiplicand exp)))))]
    (put-fn 'deriv '* deriv-product)))

(defn install-exponential-deriv
  []
  (letfn [(deriv-exponential [exp var]
            (if (exponentiation? exp)
              (make-product
               (make-product (exponent exp)
                             (make-exponentiation (base exp) (dec (exponent exp))))
               (deriv (base exp) var))))]
    (put-fn 'deriv '** deriv-exponential)))

(install-sum-deriv)
(install-product-deriv)
(install-exponential-deriv)

(prn "Part d.")
(prn "The order of arguments to the 'put' function would have to be changed, but nothing more.")







