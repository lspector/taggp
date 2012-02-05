(ns taggp.operators.crossover
  (use [taggp.globals]
       [taggp.util gp util]
       [taggp.operators select]))


(defn crossover
  [i j]
  (let [child (insert-at-index 
                i 
                (select-node i)
                (at-index j (select-node j)))]
    (if (> (depth child) @absolute-depth-limit)
      i
      child)))
