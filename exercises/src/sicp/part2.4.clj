(ns sicp.part2.4)

;; -------------------------------------------------
;; 2.4.3 Data-directed Programming and Additivitiy
;; -------------------------------------------------


;; NB in SICP in this chapter they assume two procedures 'put' and 'get' which add and read
;; from an 'operation-and-type' table. These functions are kind of fictional at the moment
;; so I will instead create a hacky implementation of them using a hash table in clojure.
