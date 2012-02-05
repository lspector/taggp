(ns taggp.operators.mutate
  (use [taggp.util gp util]
       [taggp.operators select]
       [taggp.globals]
       [taggp.random]))

(defn mutate
  [i]
  (let [child (insert-at-index 
                i 
                (select-node i)
                (random-code (ramp-depth) (if (< (rand) 0.5) :grow :full)))]
    (if (> (depth child) @absolute-depth-limit)
      i
      child)))
