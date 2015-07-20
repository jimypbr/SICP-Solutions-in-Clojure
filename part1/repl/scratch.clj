(ns part1.scratch
  (:require [clojure.core :refer :all]
            [clojure.repl :refer :all]
            [clojure.test :refer :all]
            [clojure.tools.trace :as trace]
            [clojure.math.numeric-tower :as math]
            [part1.core :refer :all] ))

(first [1 2 3])

(square 3)
(doc square)

(average 3 4)

(/ 3 4)

(improve 10 5)

(sqrt-iter 1 2)

(sqrt-v3 10)

(math/sqrt 10)

(sqrt-v2 10)

(sqrt-v3 32)

(ackermann 1 10)

(ackermann 2 4)

(ackermann 3 3)

(ackermann 4 2)

(f1-11-recur 0)
(f1-11-recur 1)
(f1-11-recur 2)
(f1-11-recur 3)
(f1-11-recur 4)
(f1-11-recur 5)
(f1-11-recur 6)
(f1-11-recur 7)
(f1-11-recur 8)

(f1-11-iter 6)
(f1-11-iter 7)
(f1-11-iter 8)


(pascal 1 1)

(expt-recur 5 4)

(even? (quot 4 2))

(expt-iter 5 4)

(defn cube [x] (* x x x))

(integral cube 0 30 0.01)

(simpsons-rule cube 0 1 1000)

(product-v2 identity 1 inc 6)

(fact 6)


((comp inc inc) 1)

(pi-approx 2)

(defn new-sum
  [f a next b]
  (accumulate-v2 + 0 f a next b))

(new-sum identity 0 inc 1000)







