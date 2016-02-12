(ns sicp.part2-5-2-test
  (:require [clojure.test :refer :all]
            [sicp.part2-5-2 :refer :all]))

;; install all the numerical packages from 2.5.2
(install-polar-package)
(install-rectangular-package)
(install-complex-package)
(install-number-package)
(install-rational-package)
(install-real-package)
(install-integer-package)

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
             (make-number 2)))))
  (testing "my numbers degrade to clojure numbers"
    (let [a (make-number 3)
          b (make-number 6)]
      (is (= (add a b)
             9))
      (is (= (sub b a)
             3))
      (is (= (mul a b)
             18))
      (is (= (div b a)
             2))
      (is (= (make-number 4)
             4))))
  (testing "equality using equ? function"
    (is (= true
           (equ? (make-number 4)
                      (make-number 4))))
    (is (= true
           (not
            (equ? (make-number 4)
                  (make-number 0))))))
  (testing "Is zero =zero?"
    (is (= true
           (=zero? 0)))
    (is (= true
           (=zero? (make-number 0))))
    (is (= false
           (=zero? (make-number 5))))))

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
             (make-rational 9 8)))))
  (testing "Equality using the equ? function"
    (let [a (make-rational 1 2)
          b (make-rational 1 2)
          c (make-rational 5 10)
          d (make-rational 1 3)]
      (is (= true (equ? a b)))
      (is (= true (equ? a c)))
      (is (= true (not (equ? a d))))))
  (testing "Is zero with =zero?"
    (is (= true
           (=zero? (make-rational 0 2))))
    (is (= false
           (=zero? (make-rational 3 4))))))

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
             (make-complex-from-real-imag -10 49)))))
  (testing "Equality using equ?"
    (let [a (make-complex-from-mag-ang 1 2)
          b (make-complex-from-mag-ang 1 2)
          c (make-complex-from-mag-ang 3 4)
          d (make-complex-from-real-imag 5 6)
          e (make-complex-from-real-imag 5 6)
          f (make-complex-from-real-imag 7 8)]
      (is (= true
             (equ? a b)))
      (is (= true
             (equ? d e)))
      (is (= false
             (equ? a c)))
      (is (= false
             (equ? d f)))))
  (testing "Is zero with =zero?"
    (is (= true
           (=zero? (make-complex-from-real-imag 0 0))))
    (is (= true
           (=zero? (make-complex-from-mag-ang 0 3))))
    (is (= false
           (=zero? (make-complex-from-real-imag 3 4))))
    (is (= false
           (=zero? (make-complex-from-mag-ang 20 3))))))


(deftest integers
  (testing "generic arithmetic with the integer type"
    (let [a (make-integer 3)
          b (make-integer 5)
          c (make-integer 100)
          d (make-integer 2)]
      (are [x y] (equ? x y)
           (make-integer 3) (make-integer 3)
           (make-integer 15) (mul a b)
           (make-integer 50) (div c d)
           (make-integer 2) (div b d)
           b (add a d)
           d (sub b a)))))



(comment
  (run-tests)
  )
