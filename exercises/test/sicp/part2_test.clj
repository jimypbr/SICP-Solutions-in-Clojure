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

(comment
  ;; insta-repl test in lighttable
  (run-tests)
)
