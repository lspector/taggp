(ns taggp.output-data.population
  (:use [taggp.util gp util]
	[taggp.globals]))

(defn best-error
  [sorted-individuals]
  (second (first sorted-individuals)))

(defn best-program
  [sorted-individuals]
  (seq (ffirst sorted-individuals)))

(defn best-program-size
  [sorted-individuals]
  (codesize (first (first sorted-individuals))))

(defn best-program-depth
  [sorted-individuals]
  (depth (first (first sorted-individuals))))

(defn median-error
  [sorted-individuals]
  (second (nth sorted-individuals (int (/ @population-size 2)))))

(defn average-program-size
  [sorted-individuals]
  (float (/ (reduce + (map codesize (map first sorted-individuals)))
	    (count sorted-individuals))))

(defn average-program-depth
  [sorted-individuals]
  (float (/ (reduce + (map depth (map first sorted-individuals)))
	    (count sorted-individuals))))

(defn tag-call-ratio
  [sorted-individuals]
  (float (/ (count (filter :tag (filter map? (flatten (map first sorted-individuals)))))
	    (count (flatten (map first sorted-individuals))))))

(defn tagged-call-ratio
  [sorted-individuals]
  (float (/ (count (filter :tagged (filter map? (flatten (map first sorted-individuals)))))
	    (count (flatten (map first sorted-individuals))))))

(defn tagged-with-args
  [sorted-individuals]
  (float (/ (count (filter :tagged-with-args (filter map? (flatten (map first sorted-individuals)))))
	    (count (flatten (map first sorted-individuals))))))

(defn unique-error-values-in-population
  [sorted-individuals]
  (count (distinct (map second sorted-individuals))))


