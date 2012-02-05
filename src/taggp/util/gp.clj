(ns taggp.util.gp
  (require [clojure.zip :as zip])
  (use [taggp.globals]))

(defn codesize [c]
  (if (seq? c)
    (count (flatten c))
    1))

(defn at-index 
  "Returns a subtree of tree indexed by point-index in a depth first traversal."
  [tree point-index]
  (let [index (mod (Math/abs point-index) (codesize tree))
        zipper (zip/seq-zip tree)]
    (loop [z zipper i index]
      (if (zero? i)
        (zip/node z)
        (if (seq? (zip/node z)) 
          (recur (zip/next (zip/next z)) (dec i))
          (recur (zip/next z) (dec i)))))))

(defn insert-at-index
  "Returns a copy of tree with the subtree formerly indexed by
   point-index (in a depth-first traversal) replaced by new-subtree."
  [tree point-index new-subtree]
  (let [index (mod (Math/abs point-index) (codesize tree))
        zipper (zip/seq-zip tree)]
    (loop [z zipper i index]
      (if (zero? i)
        (zip/root (zip/replace z new-subtree))
        (if (seq? (zip/node z))
          (recur (zip/next (zip/next z)) (dec i))
          (recur (zip/next z) (dec i)))))))

(defn annotate-points
  "Returns a sequence of [index kind] pairs where kind is :internal or :leaf."
  [tree]
  (let [limit (codesize tree)]
    (loop [z (zip/seq-zip tree) index 0 results []]
      (if (= index limit)
        results
        (if (seq? (zip/node z))
          (recur (zip/next (zip/next z))
                 (inc index) 
                 (conj results [index :internal]))
          (recur (zip/next z)
                 (inc index) 
                 (conj results [index :leaf])))))))


  