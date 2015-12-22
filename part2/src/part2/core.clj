(ns part2.core
  (:require [clojure.tools.trace :as t]))

(def x (list 1 2))
(first x)
(second x)
(last x)

;(defn make-rat [n d]
;  (list n d))

;; Exercise 2.1
(defn make-rat [n d]
  (if (< d 0)
    (list (* -1 n) (* -1 d))
    (list n d)))

;; Test cases
;; d+, n+ -> d+, n+
(make-rat 1 2)
;; d-, n+ -> d+, n-
(make-rat 1 -2)
;; d+, n- -> d+, n-
(make-rat -1 2)
;; d-, n- -> d+, n+
(make-rat -1 -2)


(defn numer [rat]
  (first rat))

(defn denom [rat]
  (second rat))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defn print-rat [rat]
  (println (str (numer rat) "/" (denom rat))))

(def one-half (make-rat 1 2))
(def one-third (make-rat 1 3))

(print-rat one-half)
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (div-rat one-half one-third))

(conj '(2) 1)


;; Exercise 2.17
(next (list 5))

(defn last-pair [coll]
  (if (next coll)
    (recur (next coll))
    coll))

(last-pair (list 34))

;; Exercise 2.18
(defn my-reverse [coll]
  (loop [c coll acc '()]
    (if c
      (recur (next c) (conj acc (first c)))
      acc)))

(my-reverse (list 1 4 9 16 25))
(my-reverse (vector 1 4))


;; Exercise 2.20
(defn same-parity [x & more]
  (let [parity (mod x 2)]
    (cons x
          (filter #(= (mod %1 2) parity) more))))

(same-parity 2 3 4 5 6 7)

;; Exercise 2.21
(defn square [x]
  (* x x))

(defn ^:dynamic square-list1 [coll]
  (if (seq coll)
    (cons (square (first coll))
          (square-list1 (rest coll)))))

(defn square-list2 [coll]
  (map #(square %1) coll))

(square-list1 (list 1 2 3 4 5))
(square-list2 (list 1 2 3 4 5))

(t/dotrace [square-list1] (square-list1 (list 1 2 3 4 5)))

;; Exercise 2.22

  (defn square-list [coll]
    (letfn [(iter [coll1 acc]
                  (if-not (seq coll1)
                    acc
                    (recur (rest coll1)
                           (conj acc (square (first coll1))))))]
      (iter coll nil)))

  (square-list (list 1 2 3 4 5))

(conj nil 0)

(cons 2 '(3))









