(ns taggp.exp.recursion
  (require [clojure.tools.logging :as log])
  (use [taggp.globals]
       [taggp.tags]))

(in-ns 'taggp.globals)

(def recursion-elimination-method (atom :none)) ;; :none, :untag, :replace, :ignore
(def recursion-location (atom :none)) ;; :tagged, :tag

(in-ns 'taggp.tags)
(use 'taggp.globals)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HANDLING RECURSION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare test-detect-recursion)
(declare test-eliminate-recursion)

(defn detect-recursion
  ^{:test test-detect-recursion}
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
  ^{:test test-eliminate-recursion
    :doc "eliminates recursion according to the policy set in taggp.globals.recursion-elimination-method

    :none     Returns the tag space as-is, without attempting to remove recursion.

    :untag    Checks to see whether any tagged code, if it were looked up using the current
      tag space, would produce recursion. If so, tag that is looked up is removed from the
      tag space.

    :replace  Replaces the tag that is found with closest-association for some-tag with a
      default value; this should be called only in cases where recursion is eliminated
      during tag lookup.

    :remove   Removes the tag that is found using closest-assocaition for some-tag
      from the tag-space."}
  (cond (= @recursion-elimination-method :none)
	   tag-space
	(= @recursion-elimination-method :untag)
	   (loop [ts tag-space]
	     (if (detect-recursion ts some-tag default-value)
	       (let [lookup (closest-association some-tag ts default-value :tag)]
		 (recur (dissoc ts lookup)))
	       ts))
	(= @recursion-elimination-method :replace)
	   (if-not (= @recursion-location :tagged)
	     (or (log/warn "recursion-location not set to :tagged ; Returning unaltered tag-space")
		 tag-space)
	     (assoc tag-space (closest-association some-tag tag-space default-value) default-value))
	(= @recursion-elimination-method :remove)
	   (if-not (= @recursion-location :tagged)
	     (or (log/warn "recursion-location not set to :tagged ; Returning unaltered tag-space")
		 tag-space)
	     (dissoc tag-space (closest-association some-tag tag-space default-value :tag))))

(defn resolve-rec [expr rec-loc default-value]
  (if (= rec-loc @recursion-location)
    #(eliminate-recursion % (rec-loc (first expr)) default-value)
    identity))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Redefine eval-with-tagging ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn eval-with-tagging
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
					       ((resolve-rec expr :tagged default-value) ts)
					       (dec step))
		 (tagdo? expr) (recur (second expr)
				      ((resolve-rec expr :tag default-value) (assoc ts (:tag (first expr)) (second expr)))
				      (dec step))
		 (tagdont? expr) [default-value
				  ((resolve-rec expr :tag default-value) (assoc ts (:tag (first expr)) (second expr)))
				  step]
		 (tagged-args? expr) (let [new-expr (clojure.walk/postwalk-replace
						     (make-argmap 10 default-value)
						     (closest-association (:tagged-with-args (first expr)) ts default-value))]
				       (recur new-expr
					      ((resolve-rec expr :tagged default-value) ts)
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
			   
