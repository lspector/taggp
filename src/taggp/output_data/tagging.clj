(ns taggp.output-data.tagging
  (use [taggp.globals]))

(defn number-of-programs-that-may-recieve-tags
  [tag-using-pgms]
  (count tag-using-pgms))

(defn number-of-these-that-exceed-limit-penalty
  [tag-using-pgms]
  (count (filter #(>= % @penalty-for-exceeding-limit) 
		 tag-using-pgms)))

