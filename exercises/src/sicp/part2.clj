(ns sicp.part2
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

;; cons always adds to the front of the list


;; Exercise 2.23
(defn for-each [f coll]
  "for-each scheme style"
  (letfn [(iter [xs]
            (when (seq xs)
              (f (first xs))
              (recur (rest xs))))]
    (iter coll)))

(defn for-each [f coll]
  "for-each clojure style"
  (doseq [x coll]
    (f x)))

(for-each (fn [x] (println x)) [1 2 3 4 5])


;; Trees
(defn pair? [xs]
  (= (count xs) 2))

(pair? '(0 2))

;; Exercise 2.24
(defn count-leaves [x]
  (cond (nil? x) 0
        (not (seq? x)) 1
        :else (+ (count-leaves (first x))
                 (count-leaves (next x)))))

(def x (cons (list 1 2) (list 3 4)))
x
(count-leaves x)
(next '(2 1))


;; Printed result: (1 (2 (3 4)))
;; Tree:
;; 1
;; --2
;; ----3
;; ----4

;; Exercise 2.25 -- get 7
(-> '(1 3 (5 7) 9)
    rest
    rest
    first
    rest
    first)

(-> '((7))
    first
    first)

(-> '(1 (2 (3 (4 (5 (6 7))))))
    rest
    first
    rest
    first
    rest
    first
    rest
    first
    rest
    first
    rest
    first)


;; Exercise 2.26
(comment
    (append x y)
      => (1 2 3 4 5 6)
    (list x y)
      => ((1 2 3) (4 5 6))
    (cons x y)
      => ((1 2 3) 4 5 6)
  )

;; Exercise 2.27
;; Tree reversal
(defn my-reverse [coll]
  (reduce conj () coll))

;; Approach 1
(defn deep-reverse1 [coll]
  (if (seq? coll)
    (concat (deep-reverse1 (next coll))
            (list (deep-reverse1 (first coll))))
    coll))

;; Approach 2 -- reverse this level then map deep-reverse
;; to children...
(defn deep-reverse2 [coll]
  (if (seq? coll)
    (my-reverse (map deep-reverse2 coll))
    coll))

(deep-reverse2 (list '(1 2) '(3 4)))

(+ (- 2 2) 2)

;; Exercise 2.28
(defn fringe [x]
  (cond
   (nil? x) '()
   (seq? (first x)) (concat (fringe (first x))
                            (fringe (next x)))
   :else (concat (list (first x))
                 (fringe (next x)))))

;; neater approach
(defn fringe2 [x]
  (if (seq? x)
    (mapcat fringe x)
    (list x)))

;; FYI idiomatic clojure way (also see source of flatten)
(defn fringe3 [x]
  (flatten x))


(def fringe-test (list (list 1 2) (list 3 4)))
(fringe2 fringe-test)
(fringe2 (list fringe-test fringe-test))


;; Exercise 2.29

;; a
(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

(defn left-branch [mobile]
  (first mobile))

(defn right-branch [mobile]
  (second mobile))

(defn branch-length [branch]
  (first branch))

(defn branch-structure [branch]
  (second branch))

(defn branch-leaf? [branch]
  (not (seq? (branch-structure branch))))

;; b
(defn total-branch-weight [branch]
  (if (branch-leaf? branch)
    (branch-structure branch)
    (+ (total-branch-weight (left-branch (branch-structure branch)))
       (total-branch-weight (right-branch (branch-structure branch))))))

(defn total-weight [mobile]
    (+ (total-branch-weight (left-branch mobile))
       (total-branch-weight (right-branch mobile))))

;; b testing
(def mobile-test1 (make-mobile (make-branch 1 1) (make-branch 1 2)))
(def mobile-test2 (make-mobile (make-branch 1 mobile-test1) (make-branch 1 1)))
(def mobile-test3 (make-mobile (make-branch 1 mobile-test2) (make-branch 1 mobile-test2)))

(total-weight mobile-test1)
(total-weight mobile-test2)
(total-weight mobile-test3)

;; c
(defn total-branch-length [branch]
  (let [structure (branch-structure branch)]
    (if (branch-leaf? branch)
      (branch-length branch)
      (+ (branch-length branch)
         (total-branch-length (left-branch structure))
         (total-branch-length (right-branch structure))))))

(defn mobile-balanced? [mobile]
  (= (* (total-branch-length (left-branch mobile))
        (total-branch-weight (left-branch mobile)))
     (* (total-branch-length (right-branch mobile))
        (total-branch-weight (right-branch mobile)))))


;; c testing
(total-branch-length (left-branch mobile-test1))
(total-branch-length (right-branch mobile-test1))
(total-branch-length (left-branch mobile-test2))
(total-branch-length (right-branch mobile-test2))
(total-branch-length (left-branch mobile-test3))
(total-branch-length (right-branch mobile-test3))
(mobile-balanced? mobile-test3)

;; d
;; By changing the implementation of mobile and branch types
;; we would only have to change the accessor functions.
;; This example doesn't translate well into clojure so I'll
;; explain in scheme:
;; In the first implementation the first element is accessed via:
;; (car mobile)
;; And the second:
;; (car (cdr mobile))
;;
;; In the second implementation the access methods for the first and
;; second elements respectively would be:
;; (car mobile)
;; (cdr mobile)
;;
;; The point is that if we sucessfully hide the implementation behind an
;; abstraction barrier, changes to the implementation only affect how
;; the publicly facing accessor functions are implemented. Any code using
;; the interface will unaffected if done properly.



















