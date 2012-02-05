;; Clojure code for tree-based genetic programming with tags
;; (see http://hampshire.edu/lspector/tags-gecco-2011/)

;; Lee Spector (lspector@hampshire.edu) 20120106-20120117

;; REQUIRES Clojure 1.3 for the concurrency to work (set single-thread-mode to true otherwise)
;; TODO
;; - make sure that using the logger doesn't write to the same stream due to lein/ant
;; - get rid of recursion in random code and make it a multimethod
;; - recursion elimination on included for tagdo true (need to add for tagdont?)
;; - add check-globals to main
;; - consider getting rid of recursive calls for arguments in taggp.tags

(ns taggp.core
  (:require [clojure.zip :as zip]
            [clojure.walk :as walk]
	    [clojure.tools.logging :as log]
	    [clojure.set :as set])
  (:use [taggp.globals]
	[taggp.operators select mutate crossover]
	[taggp.tags]
	[taggp.random]
	[taggp.util util gp]
	))
  
(defn check-globals [keys]
  (if-let [unrecognized-keys (seq (set/difference (set keys)
						  (set (map keyword (keys (ns-publics 'taggp.globals))))))]
    (log/warn (str "unrecognized-keys : " unrecognized-keys))
    'ok))

(def successful-individual? ;; Predicate to test for success (in an atom)
  (atom (fn [individual] (zero? (second individual)))))

(defn update-terminal-proportion
  []
  (reset! terminal-proportion
          (/ (count @terminal-set)
             (+ (count @terminal-set) (count (keys @function-table))))))

;; During evolution we'll maintain the population as a sequence of
;; [program error] pairs.

(defn pair-with-errors
  "Returns a vector of [program error] pairs."
  [programs]
  (println "Computing errors...")
  (vec (pmapall #(vector % (@error-fn %)) programs)))

(defn evolve []
  (println "allow-tagging =" @allow-tagging)
  (println "tagdo-semantics =" @tagdo-semantics)
  (println "use-noops =" @use-noops)
  (println "trivial-geography-radius =" @trivial-geography-radius)
  (println "population-size =" @population-size)
  (println "max-generations =" @maximum-generations)  
  (update-terminal-proportion)
  (println "Starting evolution...")
  (loop [generation 0
	 population (pair-with-errors 
		      (concat (repeatedly (/ @population-size 2) #(random-code (ramp-depth) :grow))
			      (repeatedly (/ @population-size 2) #(random-code (ramp-depth) :full))))]
    (let [sorted (sort #(< (second %1) (second %2)) population)]
      (println "Generation:" generation)
      (println "Best error:" (second (first sorted)))
      (println "Best program:" (first (first sorted)))
      (println "Best program size:" (codesize (first (first sorted))))
      (println "Best program depth:" (depth (first (first sorted))))
      (println "     Median error:" (second (nth sorted 
                                                 (int (/ @population-size 2)))))
      (println "     Average program size:" 
               (float (/ (reduce + (map codesize (map first population)))
                         (count population))))
      (println "     Average program depth:" 
               (float (/ (reduce + (map depth (map first population)))
                         (count population))))
      (println "     Tag call ratio:"
               (float (/ (count (filter :tag (filter map? (flatten (map first population)))))
                         (count (flatten (map first population))))))
      (println "     Tagged call ratio:"
               (float (/ (count (filter :tagged (filter map? (flatten (map first population)))))
                         (count (flatten (map first population))))))
      (println "     Tagged-with-args call ratio:"
               (float (/ (count (filter :tagged-with-args (filter map? (flatten (map first population)))))
                         (count (flatten (map first population))))))
      (println "     Unique error values in population:"
               (count (distinct (map second population))))
      (let [tag-using-pgms (map second (filter (fn [[p e]] 
                                                 (some (fn [item]
                                                         (or (:tagged item)
                                                             (:tagged-with-args item)))
                                                       (flatten p)))
                                               population))]
        (println "     Number of programs that may retrieve tags:" (count tag-using-pgms))
        (println "     Number of these that exceed limit penalty:"
                 (count (filter #(>= % @penalty-for-exceeding-limit) 
                                tag-using-pgms))))
      (if (@successful-individual? (first sorted))
        (println "Success:" (first (first sorted)))
        (if (>= generation @maximum-generations)
          (println "Failure")
          (recur 
            (inc generation)
            (pair-with-errors
              (for [i (range @population-size)]
                (let [operator (rand)
                      tsize @reproductive-tournament-size
                      radius @trivial-geography-radius]
                  (cond (< operator 
                           @mutation-fraction)      (mutate (select population tsize i radius))
                        (< operator 
                           (+ @mutation-fraction 
                              @crossover-fraction)) (crossover (select population tsize i radius)
                                                               (select population tsize i radius))
                        :else (select population tsize i radius)))))))))))

;(evolve)

