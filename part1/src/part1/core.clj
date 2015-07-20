(ns part1.core
  (:require [clojure.math.numeric-tower :as math]))


(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


(defn square
  "Squares a number"
  [x]
  (* x x))

;; Exercise 1.6 ---------------
;;  Defining your own 'if' function using cond.
;;  This has a gotcha due to applicative ordering in clojure and scheme.
;;  This means that the interpreter evaluates the operators and operands
;;  first and then applies the procedure to the resulting arguments.
;;  The else-clause has recursion in the example and hence is called
;;  ad infinitum.
(defn new-if
  [predicate then-clause else-clause]
    (cond
       predicate then-clause
     :else else-clause))
;;-----------------------------

;; Exercise 1.7
;;
;;  Original routine:
;;(defn good-enough?
;;  "Is square-root guess close enough?"
;;  [guess x]
;;  (< (math/abs (- (square guess) x)) 0.001))
;;
;; For small numbers you lose precision beyond 0.001. If x is
;; <= 0.001 then the routine will not return anything near the
;; correct answer. On the other hand x could be large enough that
;; the ULP of x will be > 0.001. In that case, the original
;; good-enough? will *never* evaluate as true and sqrt-iter will
;; iterate forever.

(defn good-enough?
  "Is square-root guess close enough?"
  [guess x]
  (< (math/abs (- (square guess) x)) 0.001))


;; here is a better attempt: watch how guess changes from one
;; iteration to the next and to stop when the change is a very
;; small fraction of the guess.
(defn good-enough-v2?
  "Is square-root guess close enough?"
  [old-guess guess]
  (< (math/abs (/ (- guess old-guess) old-guess)) 0.001))


(defn average
  "Average of two numbers"
  [a b]
  (double (/ (+ a b) 2)))


(defn improve
  "Improve guess by averaging guess and x/guess"
  [guess x]
  (average guess (/ x guess)))


(defn sqrt-iter
  "Do Newton-raphson iteration until
  sqrt approximation is good-enough"
  [guess x]
  (if (good-enough? guess x)
    guess
    (recur (improve guess x) x)))


(defn sqrt-iter-v2
  "Do Newton-raphson iteration until
  sqrt approximation is good-enough"
  [old-guess guess x]
  (if (good-enough-v2? old-guess guess)
    guess
    (recur guess (improve guess x) x)))


(defn sqrt-v2
  "Square root of x"
  [x]
  (sqrt-iter-v2 1 2 x))

(defn sqrt-v3
  "Square root of x"
  [x]
  (letfn [(close-enough? [guess]
            (< (math/abs (- (square guess) x)) 0.001))
          (improve' [guess]
            (average guess (/ x guess)))
          (sqrt-iter' [guess]
            (if (close-enough? guess)
              guess
              (recur (improve' guess))))]
  (sqrt-iter' 1.0)))


;; Exercise 1.10 Ackermann's function
;; (A 1 10) is 1024
;; (A 2 4) is 65536
;; (A 3 3) is 65536
;; (A 0 n) is 2n
;; (A 1 n) is 2^n
(defn ackermann [x y]
  (cond
     (= y 0) 0
     (= x 0) (* 2 y)
     (= y 1) 2
     :else (ackermann (- x 1) (ackermann x (- y 1)))))

;; Exercise 1.11
(defn f1-11-recur [n]
  (cond
     (< n 3) n
     :else (+ (f1-11-recur (- n 1))
              (* 2 (f1-11-recur (- n 2)))
              (* 3 (f1-11-recur (- n 3))))))


(defn f1-11-iter [n]
  (letfn [(iter [fn-1 fn-2 fn-3 count]
                      (if (= count (- n 3))
                        (+ fn-1 (* 2 fn-2) (* 3 fn-3))
                        (recur (+ fn-1 (* 2 fn-2) (* 3 fn-3))
                               fn-1
                               fn-2
                               (inc count))))]
    (if (< n 3)
      n
      (iter 2 1 0 0))))



;; Exercise 1.12
;; Pascal's Triangle

(defn pascal [row col]
  (cond
   (or (< row col)
       (< col 0)) (throw (Exception. "Out of Range"))
   (or (= col 0)
       (= col row)) 1
   :else (+ (pascal (- row 1) col)
            (pascal (- row 1) (- col 1)))))


;; Exercise 1.16
;; Exponentiation
(defn expt-recur
  "Returns b**n. Uses recursive successive squaring method."
  [b n]
  (cond
   (= n 0) 1
   (even? n) (square (expt-recur b (quot n 2)))
   :else (* b (expt-recur b (dec n)))))



(defn expt-iter
  "Returns b**n. Uses iterative successive squaring method."
  [b n]
  (letfn [(iter [b n a]
    (cond
     (= n 0) a
     (even? n) (recur (square b) (quot n 2) a)
     :else (recur b (dec n) (* a b) )))]
    (iter b n 1)))


;; Exercise 1.17
;; Fast multiply with addition operator

(defn double_ [x] (+ x x))
(defn halve [x] (quot x 2))

(defn mult [a b]
  (if (= b 0)
    0
    (+ a (mult a (dec b)))))


(defn fast-mult-recur [a b]
  (cond
   (= a 0) 0
   (= b 0) 0
   (even? b) (double_ (fast-mult-recur a (halve b)))
   :else (+ a (fast-mult-recur a (dec b)))))

(defn fast-mult-iter [a b]
  (letfn [(iter [a b product]
                (cond
                  (= a 0) 0
                  (= b 0) product
                  (even? b) (recur (double_ a) (halve b) product)
                  :else (recur a (dec b) (+ product a))))]
    (iter a b 0)))



;; Exercise 1.29
;; Simpson's Rule
(defn sum
  " Sum function. Takes function term(x) and sums it between
   the bounds of a and b with successive components determined by
   the next function. In iterative form as per Exercise 1.30."
  [term a next b]
  (letfn [
          (iter [a result]
                (if (> a b)
                  result
                  (recur (next a) (+ result (term a)))))]
    (iter a 0)))

(defn integral [f a b dx]
  (letfn [(add-dx [x]
            (+ x dx))]
    (* (sum f a add-dx b) dx)))

(defn simpsons-rule
  "Numerical approximation of the integral of f between a b
  using Simpson's rule with n points."
  [f a b n]
  (def h (/ (- b a) n))
  (letfn [
          (yk [a k]
            (f (+ a (* k h))))
          (simpson-term [k]
            (* (cond
                (= k n) 1
                (= k 0) 1
                (odd? k) 4
                :else 2)
               (yk a k)))]

    (* (/ h 3.0)
       (sum simpson-term 0 inc n))))


;; Exercise 1.31
;; Product
(defn product-v1
  "Product of the values of a function at points over a
  given range."
  [f a next b]
  (if (> a b)
    1
    (* (f a)
       (product-v1 f (next a) next b))))

(defn product-v2
  "Product of the values of a function at points over a
  given range. Iterative version."
  [f a next b]
  (letfn [(iter [a result]
                (if (> a b)
                  result
                  (recur (next a) (* result (f a)))))]
    (iter a 1)))

;; factorial defined using product HOF.
(defn fact [n]
  (product-v2 identity 1 inc n))


;; currently broken
(defn pi-approx [n]
  (letfn [(inc2 [x] ((comp inc inc) x))]
    (* 8
       (double (/
                (product-v2 square 4 inc2 (* 2 n))
                (product-v2 square 3 inc2 (* 2 n)))))))


;; Exercise 1.32
;; Accumulate (aka left-fold) -- even higher order functions
(defn accumulate-v1
  "Calculates the accumulation of values of a function at points
   over a given range accumulated using the combine function.
   Recursive version."
  [combiner null-value f a next b]
  (if (> a b)
    null-value
    (combiner (f a)
              (accumulate-v1 combiner null-value f (next a) next b))))

(defn accumulate-v2
  "Calculates the accumulation of values of a function at points
   over a given range accumulated using the combine function.
   Iterative version."
  [combiner null-value f a next b]
  (letfn [(iter [a result]
                (if (> a b)
                  result
                  (recur (next a) (combiner result (f a)))))]
    (iter a null-value)))
