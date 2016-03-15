(ns sicp.part2
  (:require [clojure.tools.trace :as t])
  (:require [sicp.part1 :refer [expt-iter]]))


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


;; Exercise 2.4

(defn cons1
  [x y]
  (fn [m] (m x y)))

(defn car1
  [z]
  (z (fn [p q] p)))

(defn cdr1
  [z]
  (z (fn [p q] q)))


;; Exercise 2.5

(defn cons2
  [a b]
  (* (expt-iter 2 a)
     (expt-iter 3 b)))



;; Exercise 2.17

(defn last-pair [coll]
  (if (next coll)
    (recur (next coll))
    coll))


;; Exercise 2.18
(defn my-reverse [coll]
  (loop [c coll acc '()]
    (if c
      (recur (next c) (conj acc (first c)))
      acc)))


;; Exercise 2.20
(defn same-parity [x & more]
  (let [parity (mod x 2)]
    (cons x
          (filter #(= (mod %1 2) parity) more))))


;; Exercise 2.21
(defn square [x]
  (* x x))

(defn ^:dynamic square-list1 [coll]
  (if (seq coll)
    (cons (square (first coll))
          (square-list1 (rest coll)))))

(defn square-list2 [coll]
  (map #(square %1) coll))

(defn square-list3 [coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (cons (square (first s))
           (square-list3 (rest s))))))
;(t/dotrace [square-list1] (square-list1 (list 1 2 3 4 5)))


;; Exercise 2.22

(defn square-list4 [coll]
  (letfn [(iter [coll1 acc]
                (if-not (seq coll1)
                  acc
                  (recur (rest coll1)
                         (conj acc (square (first coll1))))))]
    (iter coll nil)))
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


;; --- Trees ---


;; Exercise 2.24
(defn count-leaves [x]
  (cond (nil? x) 0
        (not (coll? x)) 1
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
(def mobile-test4 (make-mobile (make-branch 2 mobile-test3) (make-branch 4 mobile-test2)))

(total-weight mobile-test1)
(total-weight mobile-test2)
(total-weight mobile-test3)
(total-weight mobile-test4)

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
(total-branch-length (right-branch mobile-test4))
(mobile-balanced? mobile-test3)
(mobile-balanced? mobile-test4)

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


;; --- Mapping over trees ---


;; Exercise 2.30 square tree

(defn square-tree1
  "Creates a lazy-seq of a tree structured collection with the numbers
  contained squared."
  [tree]
  (lazy-seq
   (when-let [s (seq tree)]
     (if (coll? (first s))
       (cons (square-tree1 (first s))
             (square-tree1 (rest s)))
       (cons (square (first s))
             (square-tree1 (rest s)))))))

(defn square-tree2
  "Creates a lazy-seq of a tree structured collection with the numbers
  contained squared."
  [tree]
  (if (coll? tree)
    (map square-tree2 tree)
    (square tree)))

(defn square-tree3
  "Implementation of the square-tree function that preserves the input
  collection type in clojure."
  [tree]
  (cond
    (list? tree) (apply list (map square-tree3 tree))
    (seq? tree) (doall (map square-tree3 tree))
    (coll? tree) (into (empty tree) (map square-tree3 tree))
    :else (square tree)))

(defn square-tree4
  [tree]
  "Square tree using clojure.walk."
  (clojure.walk/postwalk #(if (number? %) (square %) %)
                         tree))

(square-tree4 [1 [2 [3 [4 5]]]])


;; Exercise 2.31 -- tree-map
(defn tree-map
  [f tree]
  (if (coll? tree)
    (map #(tree-map f %) tree)
    (f tree)))

(tree-map square [1 2 [3 4 [5 6]]])


;; ------- Sequences --------

;; Exercise 2.33 -- expressing things with reduce

;; I will just write these with vectors.
;; In clojure these functions like map are lazy.
;; I'm not sure how to express a lazy seq with reduce and I won't bother
;; to find out because it kind of goes beyond the scope of the question
;; and probably won't help me learn clojure...

(defn my-map [f coll]
  (reduce (fn [acc x]
            (conj acc (f x)))
          []
          coll))

(defn my-append [c1 c2]
  (reduce (fn [acc x]
            (conj acc x))
          (vec c1)
          (vec c2)))

(defn my-length [coll]
  (reduce (fn [acc _] (inc acc)) 0 coll))


;; Exercise 2.34 -- Horner's Rule

(defn horner-eval
  [x coeffs]
  (let [[c0 & cs] coeffs]
    (+ c0
       (reduce (fn [acc c]
                 (* x (+ acc c)))
               0
               (reverse cs)))))

;; Exercise 2.35 -- Count leaves
(defn count-leaves2 [tree]
  (reduce + 0
          (map (fn [t]
                 (cond
                  (nil? t) 0
                  (not (coll? t)) 1
                  :else (count-leaves2 t)))
               tree)))


;; Exercise 2.36 -- accumulate-n

(defn accumulate-n [op init colls]
  (lazy-seq
   (if (nil? (first colls))
     ()
     (cons (reduce op init (map first colls))
           (accumulate-n op init (map next colls))))))


;; Exercise 2.37 -- matrices

(defn dot-product [u v]
  (reduce + 0 (map #(* %1 %2) u v)))

(defn gemv [m v]
  (mapv #(dot-product v %) m))

(defn transpose [m]
  (into [] (accumulate-n conj [] m)))

(defn gemm [m n]
  (let [cols (transpose n)]
    (mapv #(gemv cols %) m)))


;; Exercise 2.38 -- fold-left

(defn fold-right [f initial coll]
  (if (seq coll)
    (f (first coll) (fold-right f initial (next coll)))
    initial))

(defn fold-left [f initial coll]
  (loop [c coll, acc initial]
    (if (seq c)
      (recur (next c)
             (f acc (first c)))
      acc)))

(fold-right #(conj %2 %1) [] [1 2 3 4])
(fold-left #(conj %1 %2) [] [1 2 3 4])

(fold-right / 1 [1 2 3])
(fold-left / 1 [1 2 3])
(fold-right vector [] [1 2 3])
(fold-left vector [] [1 2 3])

;; For the result of the fold-left and fold-right to be the same
;; the function passed to fold-left and fold-right must be commutative.


;; Exercise 2.39 -- reverse

(defn my-reverse2 [coll]
  (fold-right (fn [x y] (conj y x))
              []
              coll))

;; with fold-left in clojure I need to conj onto a list to get front insertion.
(defn my-reverse3 [coll]
  (into (empty coll)
        (fold-left (fn [x y] (conj x y))
                   ()
                   coll)))


;; Exercise 2.40

(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map (fn [p] (cons x p))
                          (permutations (remove #{x} s)))))
     [s])))

(defn unique-pairs [n]
  (mapcat (fn [i]
            (map (fn [j] (vector i j))
                 (range 1 i)))
          (range 1 n)))

(defn prime? [x]
  (loop [divisor 2]
    (cond
     (> (* divisor divisor) x) true
     (= 0 (mod x divisor)) false
     :else (recur (inc divisor)))))


(defn prime-sum? [pair]
  (prime? (apply + pair)))

(defn make-pair-sum [pair]
  [(first pair) (second pair) (apply + pair)])

(defn prime-sum-pairs [n]
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))


;; Exercise 2.41

(defn unique-triples [n]
  (let [n* (inc n)]
    (mapcat (fn [i]
              (mapcat (fn [j]
                        (map (fn [k] (vector i j k))
                             (range 1 j)))
                      (range 1 i)))
            (range 1 n*))))

(type (unique-triples 4))
(unique-triples 4)

(defn unique-triple-sum [n s]
  (map #(conj % {:sum (reduce + %)})
       (filter (fn [tr] (= (reduce + tr) s))
               (unique-triples n))))


(unique-triple-sum 6 12)


;; Exercise 2.42

(defn enumerate-interval [lo hi]
  (range lo (inc hi)))

(def empty-board [])

(defn adjoin-position [new-row k other-queens]
  (conj other-queens [new-row k]))

(defn left-diagonal [pos]
  (let [[r _] pos
        diff (- r 1)]
    (map #(- % diff) pos)))

(defn right-diagonal [pos]
  (let [[r c] pos
        diff (- r 1)]
    [(- r diff) (+ c diff)]))

(defn safe? [k positions board-size]
  (if-let [kth (some (fn [[row col]] (if (= col k) [row col])) positions)]
    (let [[krow kcol] kth
          k-left-diag (left-diagonal kth)
          k-right-diag (right-diagonal kth)
          other-positions (remove #{kth} positions)]
      (and
       (every? (fn [[_ c]] (not= kcol c)) other-positions)
       (every? (fn [[r _]] (not= krow r)) other-positions)
       (every? (fn [pos] (not= k-left-diag (left-diagonal pos)))
               other-positions)
       (every? (fn [pos] (not= k-right-diag (right-diagonal pos)))
               other-positions)))
    true))

(defn queens [board-size]
  (letfn [(queen-cols [k]
                  (if (= k 0)
                    (list empty-board)
                    (filter
                     (fn [positions] (safe? k positions board-size))
                     (mapcat
                      (fn [rest-of-queens]
                        (map (fn [new-row]
                               (adjoin-position new-row k rest-of-queens))
                             (enumerate-interval 1 board-size)))
                      (queen-cols (dec k))))))]
    (queen-cols board-size)))


;; --------------------------
;; Symbolic differentiation
;; --------------------------

;; using lists of symbols as representing algebraic expressions
;; ax+b => '(a * x + b)


(defn eq?
  "Are symbols s1 and s2 the same?"
  [s1 s2]
  (= s1 s2))

(defn pair?
  "Is the expression of the form '(op a b)?"
  [e]
  (and (list? e)
       (>= (count e) 3)))

(defn third
  "Third item in collection"
  [xs]
  (second (rest xs)))

(defn variable?
  "Is e a variable?"
  [e]
  (symbol? e))

(defn same-variable?
  "Are v1 and v2 the same variable?"
  [v1 v2]
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(defn sum?
  "Is e a sum?"
  [e]
  (and (pair? e)
       (eq? (first e) '+)))

(defn addend
  "Addend of the sum e"
  [e]
  (second e))

(defn augend
  "Augend of the sum e"
  [e]
  (let [len (count e)]
    (cond
     (= len 3) (third e)
     (> len 3) (conj (rest (rest e)) '+))))

(defn make-sum
  "Construct the sum of a1 and a2"
  [a1 a2]
  (cond
   (= a1 0) a2
   (= a2 0) a1
   (and (number? a1) (number? a2)) (+ a1 a2)
   :else (list '+ a1 a2)))

(defn product?
  "Is e a product?"
  [e]
  (and (pair? e)
       (eq? (first e) '*)))

(defn multiplier
  "Multiplier of the product p."
  [p]
  ;; second item of the product list
  (second p))

(defn multiplicand
  "Multiplicant of the product p"
  [p]
  (let [len (count p)]
    (cond
     (= len 3) (third p)
     (> len 3) (conj (rest (rest p)) '*))))

(defn make-product
  "Construct the product of m1 and m2"
  [m1 m2]
  (cond
   (or (= m1 0) (= m2 0)) 0
   (= m1 1) m2
   (= m2 1) m1
   (and (number? m1) (number? m2)) (* m1 m2)
   :else (list '* m1 m2)))

(defn exponentiation?
  "Is e an exponentiation?"
  [e]
  (and (pair? e)
       (eq? (first e) '**)))

(defn base
  "Base of the exponetional e"
  [e]
  (second e))

(defn exponent
  "Exponent of the exponential e"
  [e]
  (third e))

(defn make-exponentiation
  "Construct the expoentation of b**e"
  [b e]
  (cond
   (= e 0) 1
   (= b 0) 0
   (= e 1) b
   (and (number? b) (number? e))
     (expt-iter b e)
   :else (list '** b e)))

(defn deriv
  "Calculate the derivative of exp with-respect-to var"
  [exp var]
  (cond (number? exp) 0
        (variable? exp)
          (if (same-variable? exp var) 1 0)
        (sum? exp)
          (make-sum (deriv (addend exp) var)
                    (deriv (augend exp) var))
        (product? exp)
          (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp)))
        (exponentiation? exp)
          (make-product
           (make-product (exponent exp)
                         (make-exponentiation (base exp) (dec (exponent exp))))
           (deriv (base exp) var))
        :else (throw (Exception. (str "unknown expression type -- DERIV " exp)))))


;; Exercise 2.58 -- Infix notation TODO
(def precedence {'+ 0
                 '- 0
                 '* 1
                 '/ 2
                 '** 3})

(defn parse-infix
  [expr]
  (letfn [(conj-node [operators operands]
           (let [[op & _] operators
                 [a b & rest-operands] operands]
             (conj rest-operands
                   (list op b a))))
          (parse-helper
            [expr operators operands]
            (cond
              (empty? expr)
                (if (empty? operators)
                  (first operands)
                  (recur '()
                         (rest operators)
                         (conj-node operators operands)))
              (number? (first expr))
                (recur (rest expr)
                       operators
                       (conj operands (first expr)))
              (and  (symbol? (first expr))
                    (nil? (#{'+ '- '* '/ '**} (first expr))))
               (recur (rest expr)
                       operators
                       (conj operands (first expr)))
              (list? (first expr))
                (recur (rest expr)
                       operators
                       (conj operands (parse-infix (first expr))))
              :else
                (if (or (empty? operators)
                        (> (precedence (first expr))
                           (precedence (first operators))))
                  (recur (rest expr)
                         (conj operators (first expr))
                         operands)
                  (recur expr
                         (rest operators)
                         (conj-node operators operands)))))]
    (parse-helper expr '() '())))

(defn deriv-infix
  [exp var]
  (deriv (parse-infix exp) var))


(deriv-infix '(4 + 5 + x * (y + 2)) 'x)


(#{'x 'y 'z} 'p)

(deriv '(+ (* x x) (** x 3)) 'x)
