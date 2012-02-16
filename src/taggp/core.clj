;; Clojure code for tree-based genetic programming with tags
;; (see http://hampshire.edu/lspector/tags-gecco-2011/)

;; Lee Spector (lspector@hampshire.edu) 20120106-20120117

;; REQUIRES Clojure 1.3 for the concurrency to work (set single-thread-mode to true otherwise)
;; TODO
;; - recursion elimination on included for tagdo true (need to add for tagdont?)
;; - add check-globals to main
;; - consider getting rid of recursive calls for arguments in taggp.tags

(ns taggp.core
  (:require [clojure.zip :as zip]
            [clojure.walk :as walk]
	    [clojure.tools.logging :as log]
	    [clojure.string :as s]
	    [clojure.set :as set])
  (:use [taggp.globals]
	[taggp.operators select mutate crossover]
	[taggp.tags]
	[taggp.random]
	[taggp.util util gp]
	[taggp.output-data population tagging]
	))

(def globals (keys (ns-publics 'taggp.globals)))

(defn check-globals [keys]
  (if-let [unrecognized-keys (seq (set/difference (set keys)
						  (set (map keyword globals))))]
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

(defn parse-output-data-name
  [fn]
  (s/capitalize
   (str (s/join " " (s/split (if (var? fn)
			       (str (:name (meta fn)))
			       (name fn))
			     #"-"))
	":")))

(defn evolve []
  (doseq [k (sort globals)]
    (println (str k " = " @@(resolve k))))
  (update-terminal-proportion)
  (println "Starting evolution...")
  (loop [generation 0
	 population (pair-with-errors 
		      (concat (repeatedly (/ @population-size 2) #(random-code (ramp-depth) :grow))
			      (repeatedly (/ @population-size 2) #(random-code (ramp-depth) :full))))]
    (let [sorted (sort #(< (second %1) (second %2)) population)]
      (println "Generation:" generation)
      ;;; Report population and best individual data
      (doseq [data-fn (vals (ns-publics 'taggp.output-data.population))]
	(printf "%s %s\n" (parse-output-data-name data-fn) (data-fn sorted)))
      ;;; Report tag-related data
      (let [tag-using-pgms (map second (filter (fn [[p e]] 
                                                 (some (fn [item]
                                                         (or (:tagged item)
                                                             (:tagged-with-args item)))
                                                       (flatten p)))
                                               population))]
	(doseq [data-fn (vals (ns-publics 'taggp.output-data.tagging))]
	  (printf "%s %s\n" (parse-output-data-name data-fn) (data-fn tag-using-pgms))))
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

(defn parse-parameters
  "Parse parameters from command line arguments."
  ([params other]
     (println params (type params) other (type other))
     (let [params (merge other (apply hash-map (map read-string params)))]
       (doseq [[k v] params]
	 (if (contains? (set globals) (symbol (name k)))
	   (reset! (deref (resolve (symbol (name k)))) v)
	   (log/warn (str "Unrecognized key " k))))
       params))
  ([params]
     (parse-parameters params {})))