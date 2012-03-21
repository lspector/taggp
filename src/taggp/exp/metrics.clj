(ns taggp.exp.metrics
  (use [taggp.output-data.population]))

(in-ns 'taggp.output-data.population)

(defn error-vector
  [sorted-individuals]
  (frequencies (map second sorted-individuals)))