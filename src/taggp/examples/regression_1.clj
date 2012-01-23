(ns taggp.examples.regression-1
  (:use [taggp.core :exclude [-main]]))

;; 8x^3 + y^3 + 27
;(def target-data
;  (for [x (range -10 11)
;        y (range -10 11)]
;    [{'x x 'y y} 
;     (+ (* 8 x x x)
;        (* y y y)
;        27)]))

; ((xy)^4 + (xy)^2) + (16y^4 + 4y^2) + (81x^4 + 9x^2)
;(def target-data
;  (for [x (range -10 11)
;        y (range -10 11)]
;    [{'x x 'y y} 
;     (+ (* (* x y) (* x y) (* x y) (* x y))
;        (* (* x y) (* x y))
;        (* 16 y y y y)
;        (* 4 y y)
;        (* 81 x x x x)
;        (* 9 x x))]))

;; Tom's problem: (x+1)^5 with the only integer constant being 1
;(def target-data
;  (for [x (range -10 11)]
;    [{'x x} 
;     (* (inc x) (inc x) (inc x) (inc x) (inc x))]))

;; Problem for testing floats: y = x(x - 1)^2 = x^3 -2x^2 + x on range [-1, 1]
;(def target-data
;  (for [x (range -1.0 1.00001 0.02)]
;    [{'x x}
;     (* x (dec x) (dec x))]))


;; Problem for testing modularity: f1(x) = (x+1)^4 + 2^4/(3^4x^4+1) - 2^4x on range [-2, 2]
;; Difficulty: hardest
;(def target-data
;  (for [x (range -2.0 2.00001 0.04)]
;    [{'x x}
;     (+ (* (inc x) (inc x) (inc x) (inc x))
;        (/ 16
;           (inc (* 81 x x x x)))
;        (* -16 x))
;     ]))

;; Problem for testing modularity: f2(x) = x^4 + 2^4/(3^4x^4+1) on range [-2, 2]
;; Difficulty: hard
;(def target-data
;  (for [x (range -2.0 2.00001 0.04)]
;    [{'x x}
;     (+ (* x x x x)
;        (/ 16
;           (inc (* 81 x x x x))))
;     ]))

;; Problem for testing modularity: f3(x) = 2^4/(3^4x^4+1) on range [-1, 1]
;; Difficulty: moderate
;(def target-data
;  (for [x (range -1.0 1.00001 0.02)]
;    [{'x x}
;        (/ 16
;           (inc (* 81 x x x x)))
;     ]))

;; Problem for testing modularity: f4(x) = (x+2)^4 - 2^4x on range [-2, 2]
;; Difficulty: easy
(def target-data
  (for [x (range -2.0 2.00001 0.04)]
    [{'x x}
     (+ (* (inc (inc x)) (inc (inc x)) (inc (inc x)) (inc (inc x)))
        (* -16 x))
     ]))

(defn error
  "Error function for testing individuals."
  [individual]
  (reduce +' (map (fn [[bindings target]] 
                    (let [result (eval-with-tagging individual @execution-limit bindings 0)]
                      (if (= result :limit-exceeded) 
                        @penalty-for-exceeding-limit
                        (abs (-' result target)))))
                  target-data)))

(defn run
  "An evolutionary run. This is separated for REPL usage."
  []
  (reset! function-table
          (if @allow-tagging
            (zipmap '(+' -' *' pd :tagged-erf :tag-erf :tagged-with-arg-erf)
                    '(2  2  2  2  0           1         1))
            (if @use-noops
              (zipmap '(+' -' *' pd noop0 noop1 noop1)
                      '(2  2  2  2  0     1     1))
              (zipmap '(+' -' *' pd)
                      '(2  2  2  2 )))))
  (reset! terminal-set
          (if @allow-tagging
            '(x 1 arg)
            '(x 1)))
  (reset! error-fn error)
  (reset! successful-individual?
          (fn [individual]
            (< (second individual) (* @epsilon-for-float-equality (count target-data)))))	       
  (evolve))

(defn -main 
  [& params]
  (in-ns 'taggp.examples.regression-1) ;; when using lein run (= *ns* 'user) by default, we need to switch
  (let [params (merge {:allow-tagging true
                       :tagdo-semantics true
                       :use-noops true}
                      (apply hash-map (map read-string params)))]
    (println "target-data =" target-data)
    (reset! allow-tagging (:allow-tagging params))
    (reset! tagdo-semantics (:tagdo-semantics params))
    (reset! use-noops (:use-noops params))
    (run)
    (System/exit 0)))
