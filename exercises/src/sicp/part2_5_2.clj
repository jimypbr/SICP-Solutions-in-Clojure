;; this file is a dump of all the arithmetic type operations after
;; the exercises in one place.

(ns sicp.part2-5-2
  (:require [clojure.math.numeric-tower :as nmt]))

;; type dispatch table and functions ----------------

(def operation-table (atom {}))

(defn put-fn
  "Installs the item in the table, indexed by the op and the type"
  [op type item]
  (swap! operation-table conj [[op type] item]))

(defn get-fn
  "Retrieves the item in the table, indexed by the op and the type"
  [op type]
  (@operation-table [op type]))

(defn attach-tag
  "Attach a type-tag to a item. If item is a primitive then it will return
  a vector conj'd with the type-tag, unless it is a number, then it will return the number"
  [tag item]
  (cond
    (= tag :number) item    ;; for exercise 2.78
    (coll? item) (conj item tag)
    :else [item tag]))

(defn type-tag
  "Returns the type-tag of an item."
  [item]
  (if (number? item)    ;; for exercise 2.78
    :number
    (last item)))

(defn contents
  "Returns the contents of the typed item without the type-tag.
   If the item has more than one component, the components are returned
   as a vector. Otherwise the content is returned by itself."
  [item]
  (if (number? item)    ;; for exercise 2.78
    item
    (if (> (count item) 2)
      (pop item)
      (first item))))


;; type coercion table

(def coercion-table (atom {}))

(defn put-coercion
  "puts the coercion function of type1->type2 into the coercion table"
  [type1 type2 coercion-fn]
  (swap! coercion-table conj [[type1 type2] coercion-fn]))

(defn get-coercion
  "gets the coercion function for type1->type2 if it exists; else nil"
  [type1 type2]
  (@coercion-table [type1 type2]))


;; the multi-dispatching function

(comment
  ;; pre exercise 2.81
(defn apply-generic
  "Look in the operation table under the name op and the types of the
  arguments and apply the resulting procedure if it is present."
  [op & args]
  (let [type-tags (map type-tag args)
        proc (get-fn op type-tags)]
    (if proc
      (apply proc (map contents args))
      (if (= (count args) 2)
        (let [[type1 type2] type-tags
              [arg1 arg2] args
              t1->t2 (get-coercion type1 type2)
              t2->t1 (get-coercion type2 type1)]
          (cond
            t1->t2 (apply-generic op (t1->t2 arg1) arg2)
            t2->t1 (apply-generic op arg1 (t2->t1 arg2))
            :else (throw (Exception. (str "No method or these types"
                                          (list op type-tags))))))
        (throw (Exception. (str "No method for these types -- APPLY GENERIC"
                                (list op type-tags))))))))
)

(defn- all-equal? [coll]
  (let [[x & xs] coll]
    (every? #(= x %) xs)))

(comment
  ;; Pre exercise 2.84
(defn apply-generic
  "Look in the operation table under the name op and the types of the
  arguments and apply the resulting procedure if it is present."
  [op & args]
  (let [type-tags (map type-tag args)
        proc (get-fn op type-tags)]
    (if proc
      (apply proc (map contents args))
      (if (and (= (count args) 2)
               (not (all-equal? type-tags)))
        (let [[type1 type2] type-tags
              [arg1 arg2] args
              t1->t2 (get-coercion type1 type2)
              t2->t1 (get-coercion type2 type1)]
          (cond
            t1->t2 (apply-generic op (t1->t2 arg1) arg2)
            t2->t1 (apply-generic op arg1 (t2->t1 arg2))
            :else (throw (Exception. (str "No method or these types"
                                          (list op type-tags))))))
        (throw (Exception. (str "No method for these types -- APPLY GENERIC"
                                (list op type-tags))))))))

)

;; ----------------
;; Exercise 2.84
;; ----------------
;; Coercion using raising operator and numeric tower
;; + exercise 2.85 with lower function applied to the result

(def type-rank {:integer 0
                :rational 1
                :real 2
                :complex 3})

(declare raise)
(declare lower)

(defn apply-generic
  "Look in the operation table under the name op and the types of the
  arguments and apply the resulting procedure if it is present."
  [op & args]
  (let [type-tags (map type-tag args)
        proc (get-fn op type-tags)]
    (if proc
      (if (some #(= op %) ['equ? '=zero? 'raise 'project])  ;; hack to stop inf loop in lower
        (apply proc (map contents args))
        (lower (apply proc (map contents args))))
      (if (and (= (count args) 2)
               (not (all-equal? type-tags)))
        (let [[type1 type2] type-tags
              [arg1 arg2] args
              t1<t2 (< (type-rank type1) (type-rank type2))
              t2<t1 (< (type-rank type2) (type-rank type1))]
          (cond
            t1<t2 (apply-generic op (raise arg1) arg2)
            t2<t1 (apply-generic op arg1 (raise arg2))
            :else (throw (Exception. (str "No method or these types"
                                          (list op type-tags))))))
        (throw (Exception. (str "No method for these types -- APPLY GENERIC"
                                (list op type-tags))))))))



;; complex number types ----------------------------

(defn install-rectangular-package
  []
  (letfn [
          (real-part [z]
            (first z))
          (imag-part [z]
            (second z))
          (make-from-real-imag [x y]
            [x y])
          (magnitude [z]
            (sqrt (add (square (real-part z))
                            (square (imag-part z)))))
          (angle [z]
                 (arctan (div (imag-part z)
                                 (real-part z))))
          (make-from-mag-ang [r a]
            ([(mul r (cosine a)) (mul r (sine a))]))
          ;; interface to the rest of the system
          (tag [x] (attach-tag :rectangular x))]

    (put-fn 'real-part [:rectangular] real-part)
    (put-fn 'imag-part [:rectangular] imag-part)
    (put-fn 'magnitude [:rectangular] magnitude)
    (put-fn 'angle [:rectangular] angle)
    (put-fn 'make-from-real-imag :rectangular
            (fn [x y] (tag (make-from-real-imag x y))))
    (put-fn 'make-from-mag-ang :rectangular
            (fn [r a] (tag (make-from-mag-ang r a))))
    :done))

(defn install-polar-package
  []
  (letfn [(magnitude [z] (first z))
          (angle [z] (second z))
          (make-from-mag-ang [r a] [r a])
          (real-part [z]
            (mul (magnitude z) (Math/cos (angle z))))
          (imag-part [z]
            (mul (magnitude z) (Math/sin (angle z))))
          (make-from-real-imag [x y]
            (vector (Math/sqrt (add (square x) (square y)))
                    (Math/atan (div y x))))
          ;; interface to the rest of the system
          (tag [x] (attach-tag :polar x))]
    (put-fn 'real-part [:polar] real-part)
    (put-fn 'imag-part [:polar] imag-part)
    (put-fn 'magnitude [:polar] magnitude)
    (put-fn 'angle [:polar] angle)
    (put-fn 'make-from-real-imag :polar
            (fn [x y] (tag (make-from-real-imag x y))))
    (put-fn 'make-from-mag-ang :polar
            (fn [r a] (tag (make-from-mag-ang r a))))
    :done))

(defn real-part
  [z]
  (apply-generic 'real-part z))

(defn imag-part
  [z]
  (apply-generic 'imag-part z))

(defn magnitude
  [z]
  (apply-generic 'magnitude z))

(defn angle
  [z]
  (apply-generic 'angle z))

(defn make-from-real-imag
  [x y]
  ((get-fn 'make-from-real-imag :rectangular) x y))

(defn make-from-mag-ang
  [r a]
  ((get-fn 'make-from-mag-ang :polar) r a))


;; numbers --------------

(defn install-number-package
  []
  (letfn [(tag [x]
            (attach-tag :number x))]
    (put-fn 'equ? [:number :number]
            (fn [x y] (== x y)))
    (put-fn '=zero? [:number]
            (fn [x] (== x 0)))
    (put-fn 'add [:number :number]
            (fn [x y] (tag (+ x y))))
    (put-fn 'sub [:number :number]
            (fn [x y] (tag (- x y))))
    (put-fn 'mul [:number :number]
            (fn [x y] (tag (* x y))))
    (put-fn 'div [:number :number]
            (fn [x y] (tag (/ x y))))
    (put-fn 'square [:number]
            (fn [x] (tag (* x x))))
    (put-fn 'sine [:number]
            (fn [x] (tag (Math/sin x))))
    (put-fn 'cosine [:number]
            (fn [x] (tag (Math/cos x))))
    (put-fn 'arctan [:number]
            (fn [x] (tag (Math/atan x))))
    (put-fn 'sqrt [:number]
            (fn [x] (tag (Math/sqrt x))))
    (put-fn 'make :number
            (fn [x] (tag x))))
  :done)

(defn make-number [n]
  ((get-fn 'make :number) n))


(defn install-integer-package
  []
  (letfn [(tag [x]
            (attach-tag :integer x))
          (tag-real [x]
            (attach-tag :real x))]
    (put-fn 'equ? [:integer :integer]
            (fn [x y] (== x y)))
    (put-fn '=zero? [:integer]
            (fn [x] (== x 0)))
    (put-fn 'add [:integer :integer]
            (fn [x y] (tag (+ x y))))
    (put-fn 'sub [:integer :integer]
            (fn [x y] (tag (- x y))))
    (put-fn 'mul [:integer :integer]
            (fn [x y] (tag (* x y))))
    (put-fn 'div [:integer :integer]
            (fn [x y] (tag (quot x y))))
    (put-fn 'square [:integer]
            (fn [x] (tag (* x x))))
    (put-fn 'sine [:integer]
            (fn [x] (tag-real (Math/sin x))))
    (put-fn 'cosine [:integer]
            (fn [x] (tag-real (Math/cos x))))
    (put-fn 'arctan [:integer]
            (fn [x] (tag-real (Math/atan x))))
    (put-fn 'sqrt [:integer]
            (fn [x] (tag-real (Math/sqrt x))))
    (put-fn 'make :integer
            (fn [x] (tag x))))
  :done)

(defn make-integer [n]
  ((get-fn 'make :integer) (long n)))


(defn install-real-package
  []
  (letfn [(tag [x]
            (attach-tag :real x))]
    (put-fn 'equ? [:real :real]
            (fn [x y] (== x y)))
    (put-fn '=zero? [:real]
            (fn [x] (== x 0)))
    (put-fn 'add [:real :real]
            (fn [x y] (tag (+ x y))))
    (put-fn 'sub [:real :real]
            (fn [x y] (tag (- x y))))
    (put-fn 'mul [:real :real]
            (fn [x y] (tag (* x y))))
    (put-fn 'div [:real :real]
            (fn [x y] (tag (/ x y))))
    (put-fn 'square [:real]
            (fn [x] (tag (* x x))))
    (put-fn 'sine [:real]
            (fn [x] (tag (Math/sin x))))
    (put-fn 'cosine [:real]
            (fn [x] (tag (Math/cos x))))
    (put-fn 'arctan [:real]
            (fn [x] (tag (Math/atan x))))
    (put-fn 'sqrt [:real]
            (fn [x] (tag (Math/sqrt x))))
    (put-fn 'make :real
            (fn [x] (tag x))))
  :done)

(defn make-real [n]
  ;; nb applys contents to arg so incase :real :integer or :number types
  ;; are passed as an argument.
  ((get-fn 'make :real) (double (contents n))))


;; rational numbers ---------------------

(defn install-rational-package
  []
  (letfn [(numer [x] (first x))
          (denom [x] (second x))
          (make-rat [n d]
            (let [g (nmt/gcd n d)]
              [(/ n g) (/ d g)]))
          (add-rat [x y]
            (make-rat (+ (* (numer x) (denom y))
                         (* (numer y) (denom x)))
                      (* (denom x) (denom y))))
          (sub-rat [x y]
            (make-rat (- (* (numer x) (denom y))
                         (* (numer y) (denom x)))
                      (* (denom x) (denom y))))
          (mul-rat [x y]
            (make-rat (* (numer x) (numer y))
                      (* (denom x) (denom y))))
          (div-rat [x y]
            (make-rat (* (numer x) (denom y))
                      (* (denom x) (numer y))))
          (equ-rat? [x y]
            (and (= (numer x) (numer y))
                 (= (denom x) (denom y))))
          (tag [x] (attach-tag :rational x))]

    (put-fn 'equ? [:rational :rational]
            (fn [x y] (equ-rat? x y)))
    (put-fn '=zero? [:rational]
            (fn [x] (and (not (= (denom x) 0))
                         (= (numer x) 0))))
    (put-fn 'add [:rational :rational]
            (fn [x y] (tag (add-rat x y))))
    (put-fn 'sub [:rational :rational]
            (fn [x y] (tag (sub-rat x y))))
    (put-fn 'mul [:rational :rational]
            (fn [x y] (tag (mul-rat x y))))
    (put-fn 'div [:rational :rational]
            (fn [x y] (tag (div-rat x y))))
    (put-fn 'make :rational
         (fn [n d] (tag (make-rat n d)))))
  :done)

(defn make-rational
  [n d]
  ((get-fn 'make :rational) n d))


;; complex number arithmetic --------------

(defn install-complex-package
  []
  (letfn [(make-from-real-imag [x y]
            ((get-fn 'make-from-real-imag :rectangular) x y))
          (make-from-mag-ang [r a]
            ((get-fn 'make-from-mag-ang :polar) r a))
          (add-complex [z1 z2]
            (make-from-real-imag (add (real-part z1) (real-part z2))
                                 (add (imag-part z1) (imag-part z2))))
          (sub-complex [z1 z2]
            (make-from-real-imag (sub (real-part z1) (real-part z2))
                                 (sub (imag-part z1) (imag-part z2))))
          (mul-complex [z1 z2]
            (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                               (add (angle z1) (angle z2))))
          (div-complex [z1 z2]
            (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                               (sub (angle z1) (angle z2))))
          (equ-complex? [z1 z2]
            (and (= (magnitude z1) (magnitude z2))
                 (= (angle z1) (angle z2))))
          (tag [z] (attach-tag :complex z))]

    (put-fn 'equ? [:complex :complex]
            (fn [z1 z2] (equ-complex? z1 z2)))
    (put-fn '=zero? [:complex]
            (fn [z1] (== (magnitude z1) 0)))
    (put-fn 'add [:complex :complex]
            (fn [z1 z2] (tag (add-complex z1 z2))))
    (put-fn 'sub [:complex :complex]
            (fn [z1 z2] (tag (sub-complex z1 z2))))
    (put-fn 'mul [:complex :complex]
            (fn [z1 z2] (tag (mul-complex z1 z2))))
    (put-fn 'div [:complex :complex]
            (fn [z1 z2] (tag (div-complex z1 z2))))
    (put-fn 'make-from-real-imag :complex
            (fn [x y] (tag (make-from-real-imag x y))))
    (put-fn 'make-from-mag-ang :complex
            (fn [r a] (tag (make-from-mag-ang r a)))))
  :done)

(defn make-complex-from-real-imag
  [x y]
  ((get-fn 'make-from-real-imag :complex) x y))

(defn make-complex-from-mag-ang
  [r a]
  ((get-fn 'make-from-mag-ang :complex) r a))

;; generic operations on all types of complex numbers
(put-fn 'real-part [:complex] real-part)
(put-fn 'imag-part [:complex] imag-part)
(put-fn 'magnitude [:complex] magnitude)
(put-fn 'angle [:complex] angle)


;; generic operations on all types of numbers

(defn add [x y] (apply-generic 'add x y))
(defn sub [x y] (apply-generic 'sub x y))
(defn mul [x y] (apply-generic 'mul x y))
(defn div [x y] (apply-generic 'div x y))
(defn equ? [x y] (apply-generic 'equ? x y))
(defn =zero? [x] (apply-generic '=zero? x))

;; Exercise 2.86 need sine, arctan, square, sqrt, and cosine to be generic functions
(defn sine [x] (apply-generic 'sine x))
(defn cosine [x] (apply-generic 'cosine x))
(defn arctan [x] (apply-generic 'arctan x))
(defn square [x] (apply-generic 'square x))
(defn sqrt [x] (apply-generic 'sqrt x))


;; -------------------
;; Coercion functions
;; -------------------

(defn- number->complex
  [n]
  (make-complex-from-real-imag (contents n) 0))

(put-coercion :real :complex number->complex)


(comment
;; ------------
;; Testing
;; delete me

(install-polar-package)
(install-rectangular-package)
(install-complex-package)
(install-number-package)
(install-rational-package)
(install-real-package)
(install-integer-package)
)

;; --------------------------------------
;; Exercise 2.81 ------------------

;; Part a.
;; What happens when we call exp with two complex numbers as
;; arguments?
;; exp has no entry in the operation table for args [:complex :complex]
;; in apply-generic it will therefore fail the (if proc) statement. Instead
;; of failing it attempts type coercion on [:complex :complex]. It then
;; recurses on apply-generic with [:complex :complex]. Nothing has changed
;; in the inputs so the recursion is INFINTE!

;; Part b.
;; Louis is not correct that we need coercion identity functions to solve
;; this problem. apply-generic works as it is.


;; ---------------------------
;; Exercise 2.83
;; type tower procedures
;; ---------------------------


(defn raise [x]
  (apply-generic 'raise x))

(put-fn 'raise [:integer]
     (fn [x] (make-rational x 1)))

(put-fn 'raise [:rational]
        (fn [x] (make-real (/ (first x) (second x)))))

(put-fn 'raise [:real]
        (fn [x] (make-complex-from-real-imag (contents x) 0)))


;; ---------------
;; Exercise 2.85
;; ---------------

(defn project
  "generic operation that pushes a number object down the type tower"
  [x]
  (apply-generic 'project x))

(put-fn 'project [:complex]
        (fn [x] (make-real (real-part x))))

(put-fn 'project [:real]
        (fn [x] (make-integer (nmt/floor x))))

(put-fn 'project [:rational]
        (fn [x] (make-integer (nmt/floor (/ (first x) (second x))))))

(defn lower
  "projects numeric type as far down the tower as is still correct"
  [x]
  (let [type (type-tag x)]
    (cond
      (= type :number) x
      (= type :integer) x
      (equ? x (raise (project x))) (lower (project x))
      :else x)))

;; --------------
;; Exercise 2.86
;; --------------

;; We want the complex types to be composed of :real and :integer types.
;; To do this we need to change the arithmetic used in the :complex, :rectangular, and :polar
;; packages so that they use our generic arithmetic functions instead.
;; We also need to add square, sqrt, sine, cosine, and arctan generic functions to
;; the :real and :integer packages also.
;; It doesn't quite work for :rational types because the automatic demotion function
;; project breaks because it passes a :rational type to 'make-real. I'm not sure what
;; a good solution to that is yet. I don't really like the auto type demotion thing anyway :)










