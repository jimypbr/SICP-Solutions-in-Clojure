(ns sicp.part2-5_test
  (:require [clojure.test :refer :all]
            [sicp.part2-4 :refer :all]
            [sicp.part2-5 :refer :all]))


;; exercise 2.77
(deftest complex-numbers
  (testing "Generic functions and complex numbers"
    (let [a (make-complex-from-real-imag 5 6)
          b (make-complex-from-mag-ang 5 6)]
      (is (= (real-part a) 5))
      (is (= (imag-part a) 6))
      (is (= (magnitude b) 5))
      (is (= (angle b) 6)))))


(comment
  (run-tests)
  )
