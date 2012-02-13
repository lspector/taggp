(ns taggp.exp.recursion
  (use [taggp.globals]
       [taggp.tags]))


(in-ns 'taggp.globals)

(def recursion-elimination-method (atom :none)) ;; :none, :untag, :replace



(in-ns 'taggp.tags)

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
