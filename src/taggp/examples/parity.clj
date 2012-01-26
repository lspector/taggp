(ns taggp.examples.parity
  (:use [taggp.core :exclude [-main]]))

;;; even-6-parity
;
;(def target-data
;  (for [d0 [false true]
;        d1 [false true]
;        d2 [false true]
;        d3 [false true]
;        d4 [false true]
;        d5 [false true]]
;    [{'d0 d0 'd1 d1 'd2 d2 'd3 d3 'd4 d4 'd5 d5}
;     (even? (count (filter #(do %) [d0 d1 d2 d3 d4 d5])))]))

;; even-4-parity

(def target-data
  (for [d0 [false true]
        d1 [false true]
        d2 [false true]
        d3 [false true]]
    [{'d0 d0 'd1 d1 'd2 d2 'd3 d3}
     (even? (count (filter #(do %) [d0 d1 d2 d3])))]))

(defn andfn [a b] (and a b))

(defn orfn [a b] (or a b))

(defn nandfn [a b] (not (and a b)))

(defn norfn [a b] (not (or a b)))

(defn noop0 [] false)
  
(defn noop1 [a] a)

(defn error 
  [individual]
  (reduce +' (map (fn [[bindings target]] 
                    (let [result (eval-with-tagging individual @execution-limit bindings false)]
                      (if (= result :limit-exceeded) 
                        @penalty-for-exceeding-limit
                        (if (= result target) 0 1))))
                  target-data)))

(defn run
  "An evolutionary run. This is separated for REPL usage."
  []
  (reset! function-table
	  (let [basic-set (zipmap '(andfn orfn nandfn norfn)		
				  '(2     2    2      2))]
	    (if @allow-tagging
	      (merge basic-set
		     (zipmap '(:tagged-erf :tag-erf) 
			     '(0           1))
		     (when @tagged-with-args {:tagged-with-args-erf 3}))
	      (if @use-noops
		(merge basic-set
		       (zipmap '(noop0 noop1)
			       '(0     1)))
		basic-set))))	    
  (reset! terminal-set
          (let [basic-terminals '(d0 d1 d2 d3)]
	    (if (and @allow-tagging @tagged-with-args)
	      (concat '(arg0 arg1 arg2)
		      basic-terminals)
	      basic-terminals)))
  (reset! error-fn error)       
  (evolve))

(defn -main 
  [& params]
  (in-ns 'taggp.examples.parity) ;; when using lein run (= *ns* 'user) by default, we need to switch
  (let [params (merge (parse-parameters params))]
    (println "target-data =" target-data)
    (run)
    (System/exit 0)))
