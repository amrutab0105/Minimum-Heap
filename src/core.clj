
;Working Code		30/30
;
;Unit Tests		10/10
;
;Comments		9/10
; What is a heap?
;Quality of Code		5/10
; formatting -1
; Why repeat logic in too display methods
; Why mix traversal with printing? -3
; deep nested code -1
;54/60

(ns example.core
   (:require [clojure.zip :as zip])
(:gen-class))

 ;-------------------------------------------------------------------------------------------------------------------------------------------------------

;; An empty tree
(def tree-nil {:value nil :left nil :right nil})

;; Tree represented as a map.
(def map-tree {:value 45
 :left {:value 65 :left {:value 90 :left {:value 99 :left nil :right nil} :right {:value 95 :left nil :right nil}} :right {:value 81 :left nil :right nil}}
 :right {:value 72 :left {:value 82 :left nil :right nil} :right {:value 96 :left nil :right nil}}})

;; String tree represented as a map
(def map-string-tree
  {:value "ant" :left {:value "acting" :left {:value "killer" :left nil :right nil} :right {:value "coding" :left nil :right nil}}
                :right {:value "burning" :left {:value "testing" :left nil :right nil} :right nil}})

;; tree->children:
(defn tree->children
 [map]
 [(:value map) (:left map) (:right map)])

;; make-node:
(defn children->tree
 [_ sequence]
 {:value (first sequence)
 :left (second sequence)
 :right (last sequence)})

;-------------------------------------------------------------------------------------------------------------------------------------------------------

;; Making new zipper for the min-heap.
(defn heap-zipper [tree] (zip/zipper map? tree->children children->tree tree))

;; Left-Child
(defn zipper->left-child[zipper]
  (-> zipper
  zip/down
  zip/right))

;; Right-Child
(defn zipper->right-child[zipper]
  (-> zipper
  zip/down
  zip/rightmost))

;; Value
(defn zipper->value[zipper]
 (if (zip/node zipper)
 (-> zipper zip/down zip/node)
 nil))

;; Check if the tree is empty.
(defn tree-empty?[zipper]
  (not (zip/node zipper)))

;-------------------------------------------------------------------------------------------------------------------------------------------------------

;; Calculate the height of left sub-tree.
(defn height-left-tree1 [tree  h]
 (if (tree-empty? tree) h
     (recur (zipper->left-child tree) (+ 1 h))
 ))

;; Initialize the height of left sub-tree to 0 and convert the tree to zipper.
(defn height-left-tree [zipper]
(let [h 0 tree (heap-zipper zipper)]
     (height-left-tree1 tree h)))

;; Calculate the height of right sub-tree.
(defn height-right-tree1 [tree  h]
(if (tree-empty? tree) h
    (recur (zipper->right-child tree) (+ 1 h))
))

;; Initialize the height of right sub-tree to 0 and convert the tree to zipper.
(defn height-right-tree [zipper]
(let [h 0 tree (zip/zipper map? tree->children children->tree zipper)]
     (height-right-tree1 tree h)))

;-------------------------------------------------------------------------------------------------------------------------------------------------------
;; Insert at the Replace Node
(defn replace-node[zipper replacement]
 (let [ location (zip/node zipper)
       node (zip/make-node zipper location[replacement nil nil])]
 (-> zipper (zip/replace node) zip/root)))

;-------------------------------------------------------------------------------------------------------------------------------------------------------
;; Insert input node if the value is less than the current node of the sub-tree.

(defn insert-smaller [zipper tree x]; deep nesting
  (let [tree-map (heap-zipper tree) value (zipper->value zipper) root (zipper->value tree-map)]
   (if (string? value)
      (cond
          (< (compare x root) 0) (assoc tree :left (assoc {} :left (zip/node (zipper->left-child zipper)) :value value)  :value x)
          (not= (compare root value) 0) (if(> (compare x (zipper->value zipper)) 0)
                               (recur (zip/up zipper) tree x)
                               (if (< (compare x (zipper->value zipper)) 0)
                               (assoc {} :right (assoc {} :right (zip/node (zipper->right-child zipper)) :left (assoc {} :left (zip/node (zipper->left-child zipper)) :value value)
                                :value x) :left (zip/node (zipper->left-child tree-map)):value root)


                              )))


    (cond
          (< x root) (assoc tree :left (assoc {} :left (zip/node (zipper->left-child zipper)) :value value)  :value x)
          (not= root value) (if(> x (zipper->value zipper))
                               (recur (zip/up zipper) tree x)
                               (if (< x (zipper->value zipper))
                               (assoc {} :right (assoc {} :right (zip/node (zipper->right-child zipper)) :left (assoc {} :left (zip/node (zipper->left-child zipper)) :value value)
                                :value x) :left (zip/node (zipper->left-child tree-map)):value root)


                            )  )))))

;; Traverse the tree and find the position at which the input must be inserted.
(defn heap-zipper-insert[zipper tree x]; formatting
  (let [tree-map (heap-zipper tree) value (zipper->value zipper) root (zipper->value tree-map)]
    (if (string? value)
      (cond
 (tree-empty? zipper) (replace-node zipper x)
 (= (compare x value)0) (zip/root zipper)
 (> (compare x value)0) (cond
              (< (height-right-tree (zip/node zipper)) (height-left-tree (zip/node zipper))) (recur (zipper->right-child zipper) tree x)
              (= (height-right-tree (zip/node zipper)) (height-left-tree (zip/node zipper))) (recur (zipper->left-child zipper) tree x)
             )
 (< (compare x value) 0) (insert-smaller zipper tree x))


 (cond

 (tree-empty? zipper) (replace-node zipper x)
 (= x value) (zip/root zipper)
 (> x value) (cond
              (< (height-right-tree (zip/node zipper)) (height-left-tree (zip/node zipper))) (recur (zipper->right-child zipper) tree x)
              (= (height-right-tree (zip/node zipper)) (height-left-tree (zip/node zipper))) (recur (zipper->left-child zipper) tree x)
             )
 (< x value) (insert-smaller zipper tree x))
;
  )))

;-------------------------------------------------------------------------------------------------------------------------------------------------------

;; Calculate the pre-order of right sub-tree.
(defn pre-order-right [original-tree tree pre-order]
(if (tree-empty? tree)
    (recur original-tree (zip/up tree) pre-order)
    (if (tree-empty? (zipper->right-child tree))
    (recur original-tree (zip/up tree) pre-order)
    (if (= (zipper->value tree) (zipper->value original-tree) )
                      (reverse pre-order)
    (recur original-tree (zip/up tree) (conj pre-order (zipper->value (zipper->right-child tree))))))))

;; Depth first on right sub-tree (taking all the left-nodes of the right sub-tree).
(defn right->depth-first [original-tree tree pre-order]
(let [root (zipper->value original-tree) value (zipper->value tree) upper-node (zip/up tree)]
  (cond
     (= root value) (recur  original-tree (zipper->right-child tree) pre-order)
     (zip/branch? tree) (recur original-tree (zipper->left-child tree) (conj pre-order value))
     (tree-empty? tree) (pre-order-right original-tree tree pre-order)
     (tree-empty? (zipper->right-child tree)) (recur original-tree tree pre-order)
  )
 ) )



;; Calculate the pre-order of left sub-tree.
(defn pre-order-left [original-tree tree pre-order]
(if (tree-empty? tree)
    (recur original-tree (zip/up tree) pre-order)
    (if (tree-empty? (zipper->right-child tree))
    (recur original-tree (zip/up tree) pre-order)
    (if (= (zipper->value tree) (zipper->value original-tree) )
                       (right->depth-first original-tree tree pre-order)
     (recur original-tree (zip/up tree) (conj pre-order (zipper->value (zipper->right-child tree))))))))


 ;; Depth-First on left-subtree (taking all the left-nodes of the left-subtree).
(defn display [original-tree tree pre-order]
(let [root (zipper->value original-tree) value (zipper->value tree) upper-node (zip/up tree)]
  (cond
     (zip/branch? tree) (recur original-tree (zipper->left-child tree) (conj pre-order value))
     (tree-empty? tree) (pre-order-left original-tree tree pre-order))))



;-------------------------------------------------------------------------------------------------------------------------------------------------------

;; Function call to heap-zipper-insert.
(defn heap-map-insert [tree x]
(if (= nil (zipper->value (heap-zipper tree))) (replace-node (heap-zipper tree) x )
  (heap-zipper-insert (heap-zipper tree) tree x)
  )
  )
;-------------------------------------------------------------------------------------------------------------------------------------------------------

;; Display strings ending with ing.

(defn display-ing [pre-order ing]
   (cond
    (empty? pre-order)  (reverse ing)
    (= true (.endsWith (clojure.string/lower-case (first pre-order)) "ing")) (recur (rest pre-order) (conj ing (first pre-order)))
    (= false (.endsWith (clojure.string/lower-case (first pre-order)) "ing")) (recur (rest pre-order) ing)

    ))

;; Update the tree the input is provided.

(defn add-all->heap
   [heap collection]
   (if (seq collection)
      (add-all->heap (heap-map-insert heap (first collection)) (rest collection))
      (if (string? (zipper->value (heap-zipper heap)))
          (display-ing (display (heap-zipper heap) (heap-zipper heap) '()) '())
          (display (heap-zipper heap) (heap-zipper heap) '()))))

;-------------------------------------------------------------------------------------------------------------------------------------------------------

