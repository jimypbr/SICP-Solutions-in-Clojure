(ns sicp.part2-5
  (:require [clojure.math.numeric-tower :as nmt])
  (:require [clojure.tools.trace :as tr])
  (:require [sicp.part2-4 :refer [operation-table
                                  put-fn
                                  get-fn
                                  attach-tag
                                  type-tag
                                  contents
                                  apply-generic
                                  real-part
                                  imag-part
                                  magnitude
                                  angle
                                  install-rectangular-package
                                  install-polar-package]]))

;; generic operations on all types of numbers

(defn add [x y] (apply-generic 'add x y))
(defn sub [x y] (apply-generic 'sub x y))
(defn mul [x y] (apply-generic 'mul x y))
(defn div [x y] (apply-generic 'div x y))
(defn equ? [x y] (apply-generic 'equ? x y))
(defn =zero? [x] (apply-generic '=zero? x))

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
    (put-fn 'make :number
            (fn [x] (tag x))))
  :done)

(defn make-number [n]
  ((get-fn 'make :number) n))


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


;; complex numbers --------------

(defn install-complex-package
  []
  (letfn [(make-from-real-imag [x y]
            ((get-fn 'make-from-real-imag :rectangular) x y))
          (make-from-mag-ang [r a]
            ((get-fn 'make-from-mag-ang :polar) r a))
          (add-complex [z1 z2]
            (make-from-real-imag (+ (real-part z1) (real-part z2))
                                 (+ (imag-part z1) (imag-part z2))))
          (sub-complex [z1 z2]
            (make-from-real-imag (- (real-part z1) (real-part z2))
                                 (- (imag-part z1) (imag-part z2))))
          (mul-complex [z1 z2]
            (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                               (+ (angle z1) (angle z2))))
          (div-complex [z1 z2]
            (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                               (- (angle z1) (angle z2))))
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

;; install packages
(install-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

;;-----------------------------


;; ----------------
;; Exercise 2.77
;; ----------------
(comment
  ;; Problem this will fail
  (def a (make-complex-from-mag-ang 1 2))
  (real-part a)
  )

(put-fn 'real-part [:complex] real-part)
(put-fn 'imag-part [:complex] imag-part)
(put-fn 'magnitude [:complex] magnitude)
(put-fn 'angle [:complex] angle)

(comment
  ;; This now works
  (def a (make-complex-from-mag-ang 1 2))
  a
  (real-part a)
  (magnitude a)
  (map type-tag [1 2 :polar :complex])
  (map type-tag [[1 2 :polar :complex]])
  (map contents [[1 2 :polar :complex]])
  )

;; Why does this work?
;; a looks like: [1 2 :polar :complex]
;; (real-part a)
;; -> (apply-generic 'real-part a)
;; --> type-tags = (map type-tag ([1 2 :polar :complex])) = (:complex)
;; --> proc = (get-fn 'real-part (:complex)) = real-part
;; --> (apply real-part ([1 2 :polar]))
;; --> (real-part [1 2 :polar])
;; ---> (apply-generic 'real-part [1 2 :polar])
;; ----> type-tags = (map type-tag ([1 2 :polar])) = (:polar)
;; ----> proc = (get-fn 'real-part (:polar)) = real-part-polar
;; ----> (apply real-part-polar ([1 2]))
;; ----> (real-part-polar [1 2])
;; ----> FINISHED

;; apply-generic strips off the right-most type tag and gets the next function from the operation-table.
;; By mapping 'real-part to 'real-part for type [:complex] the apply-generic function gets called
;; twice: once to strip off the :complex tag and again to call the correct function on :polar/:rectangular.



;; --------------
;; Exercise 2.78
;; --------------

;; In this exercise we want to make the optimisation that for our 'boxed' number type we want it to
;; actually be clojure's own native number type. We can implement this by modifying the definitions of
;; the functions from part2_4: type-tag, contents, and attach-tag.
;;
;; type-tag:
;;   Here we need to first ask the question of whether the item is a lone number. Use clojure.core/number?
;;   to query this. If it is then return :number. Otherwise treat it as we would normally.
;;
;; contents:
;;   Same as in type-tag. Query if it is a lone number using number?. If it is, just return the number.
;;
;; attach-tag:
;;   Here we just need to query if the tag is :number. If it is return the number without any tag.
;;
;; The system works as before -- see test/sicp/part2_5_test.clj
;;
;; This exercise has quite a nice lesson in it. Quote Eli Bendersky:
;; "This code demonstrates one of the sad facts of life in programming – you often have to break your
;;  abstraction barriers for the sake of efficiency. All languages do it, and Lisp / Scheme is no different.
;;  It’s a pity that having built such a pretty, orthogonal arithmetic package, we now have to brutally
;;  slice into the generic functions that make it possible and add special cases for special values."
;; (source: http://eli.thegreenplace.net/2007/09/14/sicp-section-251)
;;
;; This problem is the same as what you get with the 'boxed number' classes in java and clojure. Similarly
;; in clojure you also have to apply some ugly but minor abstraction breaking additions to the code in
;; order to utilize the faster number primitives beneath the software abstraction layers. In this case
;; the breaks are small and they are only for a handful of cases that aren't going to grow in number as
;; you expand your system. It is therefore
