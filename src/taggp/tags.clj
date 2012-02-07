(ns taggp.tags
  (require [clojure.string :as s])
  (use [taggp.globals]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TAG UTILITIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tag?
  [inst]
  (and (map? inst)
       (= :tag (ffirst (seq inst)))))

(defn tagged?
  [inst]
  (and (map? inst)
       (or (= (ffirst (seq inst)) :tagged)
	   (= (ffirst (seq inst)) :tagged-with-args))))

(defn get-tag-id
  [inst]
  (assert (or (tag? inst) (tagged? inst)) "Not a tag instruction")
  (and (map? inst) (second (first inst))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TAG LOOKUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn closest-association
  "Returns the value for the closest match to the given tag in the given tag space, with
   default-value returned if the tag-space is empty."
  ([tag tag-space default-value return-item]
     (if (empty? tag-space)
       default-value
       (loop [associations (concat tag-space (list (first tag-space)))] ;; conj does wrap
	 (if (or (empty? (rest associations))
		 (<= tag (ffirst associations)))
	   (cond (= :tagged return-item) (second (first associations))
		 (= :tag return-item) (ffirst associations)
		 (= :both return-item) (first associations))
	   (recur (rest associations))))))
  ([tag tag-space default-value]
     (closest-association tag tag-space default-value :tagged)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HANDLING RECURSION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn detect-recursion
  [tag-space this-tag default-value]
  (let [[tag code] (closest-association this-tag tag-space default-value :both)]
    (loop [ct (count tag-space)
	   tags (map get-tag-id (filter tagged? (flatten code)))]
      (and (not (zero? ct))
	   (let [tag-refers (map #(closest-association % tag-space default-value :tag) (set tags))]
	     (or (some #(= % tag) tag-refers)
		 (recur (dec ct)
			(map get-tag-id
			     (filter tagged?
				     (flatten
				      (map #(closest-association % tag-space default-value :tagged)
					   tag-refers)))))))))))

(defn eliminate-recursion  [tag-space some-tag default-value]
  (cond (= @recursion-elimination-method :none)
	   tag-space
	(= @recursion-elimination-method :untag)
	   (loop [ts tag-space]
	     (if (detect-recursion ts some-tag default-value)
	       (let [lookup (closest-association some-tag ts default-value :tag)]
		 (recur (dissoc ts lookup)))
	       ts))
	(= @recursion-elimination-method :replace)
	   (loop [ts tag-space]
	     (if (detect-recursion ts some-tag default-value)
	       (let [lookup (closest-association some-tag ts default-value :tag)]
		 (recur (assoc ts lookup default-value)))
	       ts))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EVALUATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro make-argmap
  [n v]
  `(zipmap (map #(symbol (str "arg" %)) (quote ~(range n)))
	   (repeat (read-string (str ~v)))))

(defn constant? [expr]
  (not (seq? expr)))

(defn env-fn? [expr]
  (and (= 1 (count expr))
       (contains? (set (keys (ns-publics *ns*))) (first expr))))

(defn tagged-no-args? [expr]
  (and (= 1 (count expr))
       (map? (first expr))))

(defn tagdo? [expr]
  (and (:tag (first expr))
       @tagdo-semantics))

(defn tagdont? [expr]
  (and (:tag (first expr))
       (not @tagdo-semantics)))

(defn tagged-args? [expr]
  (and (map? (first expr))
       (not (:tag (first expr)))))

(defn if? [expr]
  (= 'if (first expr)))
		   
(defn eval-with-tagging
  "Returns the result of evaluating expression with the provided step-limit and 
   constants (which should be a map of symbols to values). The provided default-value
   is returned both for tag references that occur before any values have been tagged 
   and for tagging operations (unless tagdo-semantics is true, in which case the 
   argument to the tagging operation is evaluated and its value is returned). If the
   step-limit is exceeded then :limit-exceeded is returned. Tagging is accomplished 
   by means of an item in function position of the form {:tag n} where n is an integer,
   and where the single argument paired with this 'function' is the item to be tagged.
   Tag references look like zero-argument function calls but with a function of the 
   form {:tagged n} where n is an integer. An alternative tag reference is a one-argument
   function call with a function of the form {:tagged-with-args n}; here the code in
   the argument positions of the call will be substituted (without evaluation) for the
   symbols arg0, arg1, ... etc. in the code retrieved via tag n before branching to that
   code. In the context of boolean values the evaluator supports an 'if' form that takes
   three arguments: a condition, an if-true clause, and an if-false clause."
  ([expression step-limit constants default-value]
    (first (eval-with-tagging expression (sorted-map) step-limit constants default-value)))
  ([expression tag-space step-limit constants default-value]
     (let [constants (merge (make-argmap 10 default-value) constants)]
       ;; these calls return [value tag-space steps-remaining]
       (loop [expr expression ts tag-space step (dec step-limit)]
	 (if (<= step 0)
	   [:limit-exceeded ts step]
	   (cond (constant? expr) [(get constants expr expr) ts step]
		 (env-fn? expr) [((resolve (first expr))) ts step]
		 (tagged-no-args? expr) (recur (closest-association (:tagged (first expr)) ts default-value)
					       ts
					       (dec step))
		 (tagdo? expr) (recur (second expr)
				      (assoc ts (:tag (first expr)) (second expr))
				      (dec step))
		 (tagdont? expr) [default-value (assoc ts (:tag (first expr)) (second expr)) step]
		 (tagged-args? expr) (let [new-expr (clojure.walk/postwalk-replace
						     (make-argmap 10 default-value)
						     (closest-association (:tagged-with-args (first expr)) ts default-value))]
				       (recur new-expr
					      ts
					      (dec step)))
		 (if? expr) (let [condition-eval-result (eval-with-tagging (second expr) ts step constants default-value)]
			      (if (first condition-eval-result)
				(recur (nth expression 2) 
				       (nth condition-eval-result 1)
				       (nth condition-eval-result 2))
				(recur (nth expression 3) 
				       (nth condition-eval-result 1)
				       (nth condition-eval-result 2))))
		 :else (let [arg-evaluation-results
			     (loop [rem (rest expr) ts ts step step results []]
			       (if (empty? rem) results
				   (if (<= step 0)
				     (recur (rest rem)
					    ts
					    step
					    (conj results [:limit-exceeded ts step]))
				     (let [first-result (eval-with-tagging (first rem) ts step constants default-value)]
				       (recur (rest rem)
					      (nth first-result 1)
					      (nth first-result 2)
					      (conj results first-result))))))
			     vals (map first arg-evaluation-results)
			     ending-limit (nth (last arg-evaluation-results) 2)
			     ending-ts (nth (last arg-evaluation-results) 1)]
			 (if (<= ending-limit 0)
			   [:limit-exceeded ending-ts ending-limit]
			   [(apply (resolve (first expr)) vals) ending-ts ending-limit]))))))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; put these in the metadata for the funcitons

(let [this-tag 42
      tagged-code '(a (d b) ({:tagged 10}))
      tag-space-0 {}
      tag-space-1 (hash-map this-tag tagged-code)
      tag-space-2 (merge {12 '(a b c)} tag-space-1)
      tag-space-3 (merge {12 '(a b ({:tagged 40}))} tag-space-1)
      tag-space-4 (merge {12 '(a ({:tagged 50}) b)} tag-space-1 {54 '(a ({:tagged 40}) c)})
      tag-space-5 (merge {12 '(a ({:tagged 50}) b)} tag-space-1 {54 '(a ({:tagged 40}) ({:tagged 10}))})
      tag-space-6 (merge {11 '(a c b)} (dissoc tag-space-5 54) {54 '(a b ({:tagged 10}))})]
  (defn- test-detect-recursion []
    (assert (detect-recursion tag-space-1 this-tag :identity) tag-space-1)
    (assert (not (detect-recursion tag-space-2 this-tag :identity)) tag-space-2)
    (assert (detect-recursion tag-space-3 this-tag :identity) tag-space-3)
    (assert (detect-recursion tag-space-4 this-tag :identity) tag-space-4)
    (assert (detect-recursion tag-space-5 this-tag :identity) tag-space-5)
    (assert (not (detect-recursion tag-space-6 this-tag :identity)) tag-space-6)
    (detect-recursion tag-space-0 this-tag :identity))
  (defn- test-eliminate-recursion []
    (let [method @recursion-elimination-method]
      (reset! recursion-elimination-method :untag)
      (assert (= (eliminate-recursion tag-space-0 this-tag :identity) {}))
      (assert (= (eliminate-recursion tag-space-1 this-tag 'asdf) {}))
      (assert (= (eliminate-recursion tag-space-2 this-tag 'asdf) tag-space-2))
;;      (assert (= (eliminate-recursion tag-space-3 this-tag 'asdf) (dissoc tag-space-3 
      (println tag-space-3 this-tag)
      (eliminate-recursion tag-space-3 this-tag 'asdf)
     )))
