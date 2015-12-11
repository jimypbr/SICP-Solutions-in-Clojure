(ns sicp.part1
  (:require [clojure.math.numeric-tower :as math]))


(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


(defn square
  "Squares a number"
  [x]
  (* x x))

(defn cube
  "Cubes a number"
  [x]
  (* x x x))

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
  "Average of n numbers"
  ([& more] (/ (reduce + 0 more)
               (count more))))


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

;; Exercise 1.33
;; Filtered Accumulate
(defn filtered-accumulate
  "Calculates the accumulation of values of a function at points
   over a given range accumulated using the combine function if
   they satisfy the filter condition."
  [valid? combiner null-value f a next b]
  (if (> a b)
    null-value
    (if (valid? a)
      (combiner (f a)
                (filtered-accumulate valid? combiner null-value
                                     f (next a) next b))
      (filtered-accumulate valid? combiner null-value
                           f (next a) next b))))

;; part b
(defn ex-1-33b
  "Product of all positive integers i<n such that gcd(i,n)==1"
  [n]
  (filtered-accumulate (fn [i] (= 1 (math/gcd i n)))
                       * 1 identity 1 inc n))



;; Exercise 1.34
(defn f [g] (g 2))

(f square)
(f (fn [z] (* z (+ z 1))))

;; If we call (f f) we get:
;; (f f)
;; (f 2)
;; (2 2)
;; Exception due to trying to call 2 (an atom in scheme)
;; like it was a function.

(neg? 0)


;; Exercise 1.35
;; Golden ratio is x that satisfies the equation x^2 = x + 1
;; Fixed point of f is y such that f(y) = y
;; Uding the Golden Ratio equation we get that:
;; f(y) = y = (y+1) / y = 1 + 1/y
;; So the fixed point of the transformation y -> 1 + 1/y
;; is x the Golden Ratio.

(def tolerance 0.00001)
(defn fixed-point [f first-guess]
  (letfn [(close-enough? [v1 v2]
            (< (Math/abs (- v1 v2)) tolerance))
          (try1 [guess]
            (let [next (f guess)]
              ;;(println next)
              (if (close-enough? guess next)
                next
                (recur next))))]

    (try1 first-guess)))

;; Note Math/cos is a Java class method => *not* first class!
;; Need to wrap it in a clojure function.
(fixed-point #(Math/cos %) 1.0)
(fixed-point #(+ (Math/cos %) (Math/sin %)) 1.0)
(fixed-point #(+ 1 (/ 1.0 %)) 1.0)

;; Exercise 1.36
(fixed-point #(average % (/ 2.0 %)) 1.0)
(fixed-point #(/ (Math/log 1000) (Math/log %)) 2)
(fixed-point #(average % (/ (Math/log 1000) (Math/log %))) 2)

;; Using average damping it converges in 9 steps
;; Without average damping it converges in ~30 steps.

;; Exercise 1.37
;; Continued Fractions
;;(defn cont-frac [n d k]
; (let [kth-n (n k)
;       kth-d (d k)]
;   (if (zero? k)
;     (/ kth-n kth-d)
;     (/ kth-n (+ kth-d (cont-frac n d (dec k)))))))

(defn cont-frac [n d k]
  "Kth order contined fraction. n and d are functions of k that return
  the kth numerator and denominator respectively. Recursive implementation."
  (letfn [(cont-frac-recur [i]
           (let [ith-n (n i)
                 ith-d (d i)]
             (if (= i k)
               (/ ith-n ith-d)
               (/ ith-n (+ ith-d (cont-frac-recur (inc i)))))))]
    (cont-frac-recur 1)))

(defn cont-frac-1 [n d k]
  "Kth order contined fraction. n and d are functions of k that return
  the kth numerator and denominator respectively. Iterative implementation."
  (letfn [(cont-frac-iter [i result]
             (if (= i 0)
               result
               (recur (dec i) (/ (n i) (+ (d i) result)))))]
    (cont-frac-iter k 0.0)))


(def golden-ratio (/ (+ 1 (math/sqrt 5.0)) 2.0))
(/ 1.0 (cont-frac (fn [i] 1.0) (fn [i] 1.0) 100))
(/ 1.0 (cont-frac-1 (fn [i] 1.0) (fn [i] 1.0) 100))

(defn find-k [k]
  (letfn [(close-enough? [v1 v2]
          (< (Math/abs (- v1 v2))
             tolerance))]
    (let [next (/ 1.0 (cont-frac-1 (fn [i] 1.0) (fn [i] 1.0) k))]
      (if (close-enough? golden-ratio next)
        k
        (recur (inc k))))))
(find-k 1)

;; To get Phi within 4 decimal places requires k >= 13


;; Exercise 1.38
;; Use Euler's continued fraction to approximate e-2.

(defn euler-n [i] 1)

;; Use lazy sequence to define Euler's sequence for the denominators!!
(defn euler-d [i]
  (letfn [(euler-seq
           ([] (cons 1 (euler-seq 1)))
           ([n] (concat [(* 2 n) 1 1] (lazy-seq (euler-seq (inc n))))))]
    (nth (euler-seq) (dec i))))

;; Approximately e:
(+ (cont-frac-1 euler-n euler-d 30) 2)

;; Exercise 1.39
;; Calculate tan x using JH Lambert's continued fraction.

(defn tan-cf [x k]
  (cont-frac-1
   (fn [i]
     (if (= i 1) x (- (square x))))
   (fn [i]
     (if (= i 1) 1 (+ i (dec i))))
   k))

(tan-cf 1.5707 20)


;; Newton's Method for Root Finding

(def dx 0.00001)

(defn deriv [g]
  (fn [x] (/ (- (g (+ x dx))
                (g x))
             dx)))

(defn newton-transform [g]
  (fn [x] (- x (/ (g x) ((deriv g) x)))))

(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))

(defn sqrt-v4 [x]
  (newtons-method
   (fn [y] (- (square y) x)) 1.0))

(sqrt-v4 2)


;; Exercise 1.40
(defn cubic [a b c]
  (fn [x]
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(newtons-method (cubic 0 1 1) 1)

;; Exercise 1.41
(defn double1 [f]
  (fn [x]
    ((comp f f) x)))

((double1 inc) 2)

;; Exercise 1.42
(defn compose [f g]
  (fn [x]
    (f (g x))))

((compose square inc) 6)

;; Exercise 1.43
(defn repeated [f n]
  (fn [x]
    (nth (iterate #(f %) x) n)))

((repeated #(+ 1 %) 2) 5)
((repeated square 2) 5)

(square (square (square (square (square 2)))))

;; Exercise 1.44
(defn smooth [f]
  (fn [x]
    (average
     (f (- x dx))
     (f x)
     (f (+ x dx)))))

(defn n-fold-smooth [f n]
  (fn [x]
    ((repeated (smooth f) n) x)))

((n-fold-smooth #(/ 10 %) 1) 3)


;; Exercise 1.45
(defn average-damp [f]
  (fn [x]
    (average x (f x))))

(defn sqrt-v5 [x]
  (fixed-point (average-damp (fn [y] (/ x y)))
               1.0))


(defn log2 [x]
  (/ (Math/log x) (Math/log 2)))

(defn nth-root [x n]
  (fixed-point
   ((repeated average-damp (Math/floor (log2 n)))
    (fn [y] (/ x (expt-iter y (dec n)))))
   1.0))


;; Exercise 1.46
;; Iterative improve
;;
;; To compute something, we start with an initial guess for the answer,
;; test if the guess is good enough, and otherwise improve the guess and
;; continue the process using the improved guess as the new guess.

(defn iterative-improve [good-enough? improve]
  (fn [guess]
    (if (good-enough? guess)
      guess
      (recur (improve guess)))))

(defn sqrt-v6 [x]
  (letfn [(good-enough? [guess]
            (< (/ (math/abs (- (square guess) x))
                  x) 0.000001))
          (improve [guess]
            (average guess (/ x guess)))]
    ((iterative-improve good-enough? improve) 1.0)))

(sqrt-v6 2)

(defn fixed-point-v2 [f first-guess]
  (letfn [(close-enough? [guess]
            (< (/ (math/abs (- (f guess) guess))
                  guess) 0.000001))
          (improve [guess]
            (f guess))]
    ((iterative-improve close-enough? improve) first-guess)))

(fixed-point-v2 #(Math/cos %) 1.0)


