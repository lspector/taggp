(ns taggp.examples.parity
  (:require [clojure.tools.logging :as log])
  (:use [taggp.core :exclude [-main]]
	[taggp.globals]
	[taggp.tags]))


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
  (reset! node-selection-method :tournament)
  (reset! function-table
          (if @allow-tagging
            (zipmap '(andfn orfn nandfn norfn :tagged-erf :tag-erf) ;:tagged-with-args-erf)
                    '(2     2    2      2     0           1       )); 3))
            (if @use-noops
              (zipmap '(andfn orfn nandfn norfn noop0 noop1 noop1)
                      '(2     2    2      2     0     1     1))
              (zipmap '(andfn orfn nandfn norfn)
                      '(2     2    2      2    )))))
  (reset! terminal-set
          (if @allow-tagging
            '(d0 d1 d2 d3 ) ;arg0 arg1 arg2) ;; also include d4 and d6 for 6-parity
            '(d0 d1 d2 d3)))
  (reset! error-fn error)       
  (evolve))

(defn -main 
  [& params]
  (in-ns 'taggp.examples.parity) ;; when using lein run (= *ns* 'user) by default, we need to switch
;;;; How to use exp 
  (use 'taggp.exp.recursion)
  (use 'taggp.globals :reload)
  (use 'taggp.tags :reload)
  (use 'taggp.core :reload)
;;;;
  (set! *warn-on-reflection* true)
  (let [params (merge {:allow-tagging true
                       :tagdo-semantics true
                       :use-noops true}
                      (apply hash-map (map read-string params)))]
    (println "target-data =" target-data)
    (doseq [[k v] params]
      (if (contains? (set globals) (symbol (name k)))
	(reset! (deref (resolve (symbol (name k)))) v)
	(log/warn (str "Unrecognized key " k)))))
    (run)
    (System/exit 0))
