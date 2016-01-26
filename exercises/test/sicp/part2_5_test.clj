(ns sicp.part2-5_test
  (:require [clojure.test :refer :all]
            [sicp.part2-4 :refer :all]
            [sicp.part2-5 :refer :all]))


(deftest number-package
  (testing "Basic arithmetic on numbers."
    (let [a (make-number 3)
          b (make-number 6)]
      (is (= (add a b)
             (make-number 9)))
      (is (= (sub b a)
             (make-number 3)))
      (is (= (mul a b)
             (make-number 18)))
      (is (= (div b a)
             (make-number 2))))))

(deftest rational-package
  (testing "Arithmetic on rationals"
    (let [a (make-rational 10 5)
          b (make-rational 1 5)
          c (make-rational 2 3)
          d (make-rational 3 4)]
      (is (= a (make-rational 2 1)))
      (is (= (add b c)
             (make-rational 13 15)))
      (is (= (add c d)
             (make-rational 17 12)))
      (is (= (sub c b)
             (make-rational 7 15)))
      (is (= (mul c d)
             (make-rational 1 2)))
      (is (= (div d c)
             (make-rational 9 8))))))

;; exercise 2.77
(deftest complex-numbers
  (testing "Generic functions and complex numbers"
    (let [a (make-complex-from-real-imag 5 6)
          b (make-complex-from-mag-ang 5 6)]
      (is (= (real-part a) 5))
      (is (= (imag-part a) 6))
      (is (= (magnitude b) 5))
      (is (= (angle b) 6))))
  (testing "Arithmetic on complex numbers"
    (let [a (make-complex-from-real-imag 4 5)
          b (make-complex-from-real-imag 5 6)]
      (is (= (add a b)
             (make-complex-from-real-imag 9 11)))
      (is (= (sub a b)
             (make-complex-from-real-imag -1 -1)))
      (is (= (mul a b)
             (make-complex-from-real-imag -10 49))))))




(comment
  (run-tests)
  )
