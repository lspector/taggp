(ns taggp.util.util
  (use [taggp.globals]))

(defn pmapall
  "Like pmap but: 1) coll should be finite, 2) the returned sequence
   will not be lazy, 3) calls to f may occur in any order, to maximize
   multicore processor utilization, and 4) takes only one coll so far."
  [f coll]
  (if @single-thread-mode
    (map f coll)
    (let [agents (map #(agent % :error-handler (fn [agnt except] (println except))) coll)]
      (dorun (map #(send % f) agents))
      (apply await agents)
      (doall (map deref agents)))))

(defn float-zero?
  "Tests if a float is close enough to zero to count as zero for evolution."
  [x]
  (and (> x (- @epsilon-for-float-equality))
       (< x @epsilon-for-float-equality)))

(defn pd
  "Protected biginteger or float division; returns 1 if the denominator is zero."
  [num denom]
  (if @integer-regression
    (if (zero? denom)
      1
      (bigint (quot num denom)))
    (if (float-zero? denom)
      1.0
      (float (/ num denom)))))
  
(defn abs ;; works even for bigints
  [x]
  (if (< x 0) (- x) x))


(defn depth
  [expression]
  (if (not (seq? expression))
    0
    (let [subsequences (filter seq? expression)]
      (if (empty? subsequences)
        1
        (inc (apply max (map depth subsequences)))))))

