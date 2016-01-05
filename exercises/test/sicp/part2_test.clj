(ns sicp.part2_test
  (:require [clojure.test :refer :all]
            [sicp.part2 :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))


(deftest e2.30a
  (testing "Ex 2.30a: square-tree"
    (is (=
         (square-tree1
          (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))
         '(1 (4 (9 16) 25) (36 49))))
    (is (= (square-tree1 '()) '()))
    (is (= (square-tree1 [1 2 3 4 5]) [1 4 9 16 25]))
    (is (= (square-tree1 [1 [2 [3]]]) [1 [4 [9]]]))
    ))

(deftest e2.30b
  (testing "Ex 2.30b: square-tree with type preservation"
    (is (=
         (square-tree3
          (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))
         '(1 (4 (9 16) 25) (36 49))))
    (is (= (square-tree3 '()) '()))
    (is (= (square-tree3 [1 2 3 4 5]) [1 4 9 16 25]))
    (is (= (square-tree3 [1 [2 [3]]]) [1 [4 [9]]]))
    (is (= (type (square-tree3 [1 [2 [3]]]))
           (type [1 [4 [9]]])))
    ))

(deftest fold-left-test
  (testing "fold-left"
    (is (=
         (fold-left + 0 (list 1 2 3 4 5))
         15))
    (is (=
         (fold-left * 1 (list 1 2 3 4 5))
         120))
    (is (=
         (fold-left conj [] (list 1 2 3 4 5))
         (vector 1 2 3 4 5)))
    (testing "with big input collections"
      (let [n 10000000]
        (is (=
             (fold-left + 0 (range n))
             (/ (* n (- n 1)) 2)))))
  ))


(deftest ex2.33
  (testing "map written with reduce"
    (is (=
         (my-map identity (range 1 10))
         (map identity (range 1 10)))))
  (testing "append written with reduce"
    (is (=
         (my-append [1 2 3 4] [5 6 7 8])
         [1 2 3 4 5 6 7 8])))
  (testing "length written with reduce"
    (is (=
         (my-length [1 2 3 4 5 6 7 8])
         8))))

(deftest ex2.34
  (testing "Horner's Rule"
    (is (=
         (horner-eval 2 (vector 1 3 0 5 0 1))
         79))))

(deftest ex2.36
  (testing "accumulate-n"
    (is (=
         (accumulate-n + 0 [[1 2 3] [4 5 6] [7 8 9] [10 11 12]])
         [22 26 30]))))

(deftest ex2.37
  (testing "dot-product"
    (is (=
         (dot-product [1 2 3] [4 5 6])
         32)))
  (testing "matrix * vector"
    (is (=
         (gemv [[1 2] [3 4]] [5 6])
         [17 39])))
  (testing "transpose"
    (is (=
         (transpose [[1 2] [3 4]])
         [[1 3] [2 4]]))
    (is (=
         (transpose [[1 2 3] [4 5 6] [7 8 9]])
         [[1 4 7] [2 5 8] [3 6 9]])))
  (testing "matrix * matrix"
    (is (=
         (gemm [[1 2] [1 2]] [[3 4] [3 4]])
         [[9 12] [9 12]]))
    (is (=
         (gemm [[1 2] [3 4]] [[5 6] [7 -8]])
         [[19 -10] [43 -14]]))))

(deftest fold-right-test
  (testing "fold-right"
    (is (=
         (fold-right + 0 (list 1 2 3 4 5))
         15))
    (is
     (=
      (fold-right / 1 [1 2 3])
      (/ 1 (/ 2 3))))
    (is
     (=
      (fold-right vector [] [1 2 3])
      [1 [2 [3 []]]]))))

(deftest fold-left-test
  (testing "fold-left"
    (is
     (=
      (fold-left + 0 [1 2 3 4 5])
      15))
    (is
     (=
      (fold-left / 1 [1 2 3])
      (/ (/ 1 2) 3)))
    (is
     (=
      (fold-left vector [] [1 2 3])
      [[[[] 1] 2] 3]))))


(deftest k-queens-test
  (testing "adjoin-position"
    (is (=
         (adjoin-position 4 7 [[1 6] [2 3] [3 1]])
         [[1 6] [2 3] [3 1] [4 7]])))
  (testing "left-diagonal"
    (is (= (left-diagonal [1 1])
           [1 1]))
    (is (= (left-diagonal [2 3])
           [1 2]))
    (is (= (left-diagonal [3 4])
           [1 2]))
    (is (= (left-diagonal [4 4])
           [1 1]))
    (is (= (left-diagonal [4 2])
           (left-diagonal [3 1])))
    (is (= (left-diagonal [4 3])
           (left-diagonal [2 1])
           (left-diagonal [3 2])))
  (testing "right-diagonal"
    (is (= (right-diagonal [2 1])
           [1 2]))
    (is (= (right-diagonal [3 1])
           [1 3]))
    (is (= (right-diagonal [4 1])
           [1 4]))
    (is (= (right-diagonal [1 1])
           [1 1]))
    (is (= (right-diagonal [4 2])
           (right-diagonal [3 3])
           (right-diagonal [2 4])))))
  (testing "safe?"
    (is (= (safe? 2 [[2 2] [1 4]] 4)
           true))
    (is (= (safe? 1 [[1 2] [2 1]] 2)
           false)))
  (testing "Number of Queens"
    (is (every? #(= true %)
                (map #(= (count (queens %1)) %2)
                     (range 1 9)
                     [1 0 0 2 10 4 40 92])))))


(deftest symbolic-deriv
  (testing "exponentiation"
    (is (= true (exponentiation? '(** 2 3))))
    (is (= false (exponentiation? '(* 3 4))))
    (is (= 2 (base '(** 2 3))))
    (is (= 3 (exponent '(** 2 3))))
    (is (= '(** b 23) (make-exponentiation 'b 23)))
    (is (= 1 (make-exponentiation 'b 0)))
    (is (= 'b (make-exponentiation 'b 1)))
    (is (= 0 (make-exponentiation 0 3)))
    (is (= 32 (make-exponentiation 2 5)))
    )
  (testing "deriv exponentential"
    (is (= '(* 3 (** x 2))
           (deriv '(** x 3) 'x)))
    (is (= 0 (deriv '(** x 0) 'x)))
    )
  (testing "Longer expressions +"
    (is (= '(+ y z) (augend '(+ x y z))))
    (is (= '(+ x y z) (augend '(+ w x y z))))
    (is (= 'y (augend '(+ x y))))
    (is (= 'z (augend (augend (augend '(+ w x y z)))))))
  (testing "Long expressions *"
    (is (= '(* y z) (multiplicand '(* x y z))))
    (is (= '(* x y z) (multiplicand '(* w x y z))))
    (is (= 'y (multiplicand '(* x y)))))
  (testing "Deriv longer expressions"
    (is (= '(+ (* x y) (* y (+ x 3)))
           (deriv '(* x y (+ x 3)) 'x)))))

(comment
  ;; insta-repl test in lighttable
  (run-tests)
)
