(ns taggp.operators.select
  (use [taggp.util gp]
       [taggp.globals]))

(defn select
  "Returns the program of the best [program error] pair in a tournament
   set with the specified size, location, and radius."
  [prog-err-pairs tournament-size location radius]
  (let [limit (count prog-err-pairs)
        tournament-set (repeatedly 
                         tournament-size 
                         #(nth prog-err-pairs
                               (mod (+ location 
                                       (- (rand-int (inc (* radius 2))) 
                                          radius))
                                    limit)))]
    (first (first (sort #(< (second %1) (second %2)) (vec tournament-set))))))


(defn select-node-90-10
  "returns an index"
  [tree]
  (let [annotated (annotate-points tree)
        internals (map first (filter #(= (second %) :internal) annotated))
        leaves (map first (filter #(= (second %) :leaf) annotated))]
    (cond (empty? internals) (rand-nth leaves)
          (empty? leaves)    (rand-nth internals)
          (< (rand) 0.9)     (rand-nth internals)
          :else              (rand-nth leaves))))


(defn select-node-by-tournament
  "returns an index"
  [tree]
  (let [num-nodes (codesize tree)
        tournament-set (repeatedly @node-tournament-size #(rand-int num-nodes))]
    (ffirst (sort #(> (second %1) (second %2))
                  (map #(vector % (codesize (at-index tree %))) tournament-set)))))
    
(defn select-node
  [tree]
  (case @node-selection-method
    :tournament (select-node-by-tournament tree)
    :koza (select-node-90-10 tree)
    ))


