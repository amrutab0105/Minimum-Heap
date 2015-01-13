(ns example.core-test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]
 ))

(defn reload-tests [ ]
 (use 'example.core :reload-all)
 (use 'example.core-test :reload-all)
 (run-tests 'example.core-test))

(reload-tests)

(deftest add-all->heap-test
 (testing "add-all->heap"
 (are [answer heap collection] (= answer (add-all->heap heap collection))

;adding 5 to the given tree.
[5 45 65 90 99 95 81 72 82 96] map-tree [5]

;adding 100 to the given tree.
[5 45 65 90 99 95 81 72 82 100 96] map-tree [5 100]

;adding 50 to the given tree.
[5 45 65 90 99 95 81 50 72 82 100 96] map-tree [5 100 50]

;adding values to an empty tree.
[1 2 3] tree-nil [1 2 3]

;creating a tree as you traverse it.
[10 45 65 99 81 90 95] tree-nil [45 65 90 99 95 81 10]

;printing string ending with ing in pre-order.
["acting" "coding" "burning" "testing"] map-string-tree []

;adding a string
["acting" "coding" "burning" "testing" "compiling"] map-string-tree ["compiling"]

)))

map-tree

map-string-tree

tree-nil

;printing the result of integer tree.
(println (add-all->heap map-tree [5 100]))

;printlning the result of string tree.
(println (add-all->heap map-string-tree ["liking"]))


