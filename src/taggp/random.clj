(ns taggp.random
  (use [taggp.globals]))
;; random code generator from the GP field guide p 14

(defn expand-erc
  [item]
  (cond (= item :int-erc) (+ (first @int-erc-range) 
                             (rand-int (inc (- (second @int-erc-range)
                                               (first @int-erc-range)))))
        (= item :float-erc) (+ (first @float-erc-range) 
                               (* (rand)
                                  (- (second @float-erc-range)
                                     (first @float-erc-range))))
        :else item))

(defn expand-erf
  [item]
  (cond (= item :tag-erf) {:tag (rand-int @tag-limit)}
        (= item :tagged-erf) {:tagged (rand-int @tag-limit)}
        (= item :tagged-with-args-erf) {:tagged-with-args (rand-int @tag-limit)}
        :else item))


(defn random-code 
  [depth-limit method] ;; method should be :grow or :full
  (if (or (= depth-limit 0)
          (and (= method :grow)
               (< (rand) @terminal-proportion)))
    (expand-erc (rand-nth @terminal-set))
    (let [f (expand-erf (rand-nth (keys @function-table)))]
      (cons f (repeatedly (if (map? f)
                            (cond (:tagged f) 0 
                                  (:tag f) 1
                                  (:tagged-with-args f) (get @function-table :tagged-with-args-erf))
                            (get @function-table f))
                          #(random-code (dec depth-limit) method))))))


(defn ramp-depth [] (rand-nth (apply range @ramp-depth-range)))
