(ns sicp.part2-5
  (:require [clojure.math.numeric-tower :as nmt])
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

(defn add [x y] (apply-generic 'add x y))
(defn sub [x y] (apply-generic 'sub x y))
(defn mul [x y] (apply-generic 'mul x y))
(defn div [x y] (apply-generic 'div x y))

(defn install-number-package
  []
  (letfn [(tag [x]
            (attach-tag :number x))]
    (put-fn 'add [:number :number]
            (fn [x y] (tag (+ x y))))
    (put-fn 'sub [:number :number]
            (fn [x y] (tag (- x y))))
    (put-fn 'mul [:number :number]
            (fn [x y] (tag (* x y))))
    (put-fn 'div [:number :number]
            (fn [x y] (tag (/ x y))))
    (put-fn 'make :number
            (fn [x] (tag [x]))))
  :done)

(defn make-number [n]
  ((get-fn 'make :number) n))

(defn install-rational-package
  []
  (letfn [(numer [x] (first x))
          (denom [x] (second x))
          (make-rat [n d]
            (let [g (nmt/gcd n d)]
              [(/ n g) (/ d g)]))
          (add-rat [x y]
            (make-rat (+ (* (numer x) (denom y))
                         (* (numer y) (denom y)))
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
          (tag [x] (attach-tag :rational x))]
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
          (tag [z] (attach-tag :complex z))]
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


(make-number 1)
(make-rational 1 2)
(make-complex-from-real-imag 1 2)
(make-complex-from-mag-ang 1 2)
