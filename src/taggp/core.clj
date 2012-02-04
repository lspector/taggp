;; Clojure code for tree-based genetic programming with tags
;; (see http://hampshire.edu/lspector/tags-gecco-2011/)

;; Lee Spector (lspector@hampshire.edu) 20120106-20120117

;; REQUIRES Clojure 1.3 for the concurrency to work (set single-thread-mode to true otherwise)

(ns taggp.core
  (:require [clojure.zip :as zip]
            [clojure.walk :as walk]))

(def single-thread-mode (atom false))
(def absolute-depth-limit (atom 17))
(def population-size (atom 1000))
(def trivial-geography-radius (atom 500))
(def maximum-generations (atom 50))
(def reproductive-tournament-size (atom 7))
(def mutation-fraction (atom 0.05))
(def crossover-fraction (atom 0.9))

(def allow-tagging (atom true))
(def tag-limit (atom 1000))
(def tagdo-semantics (atom true))
(def use-noops (atom true)) ;; only has an effect if allow-tagging is false

(def execution-limit (atom 1000))
(def penalty-for-exceeding-limit (atom 10000000000000N))

(def node-selection-method (atom :koza)) ;; should be :koza or :tournament
(def node-tournament-size (atom 2))

(def int-erc-range (atom [-10 10]))
(def float-erc-range (atom [-10 10]))
(def ramp-depth-range (atom [2 6])) ;; for ramped half and half tree generation

(def epsilon-for-float-equality (atom 0.01)) ;; for detecting float division by zero
(def integer-regression (atom false))

(def function-table (atom {}))
(def terminal-set (atom ()))
(def terminal-proportion (atom 0));; note: update-terminal-proportion
(def error-fn (atom (fn [individual] 0)))

(def successful-individual? ;; Predicate to test for success (in an atom)
  (atom (fn [individual] (zero? (second individual)))))

(defn closest-association
  "Returns the value for the closest match to the given tag in the given tag space, with
   default-value returned if the tag-space is empty."
  [tag tag-space default-value]
  (if (empty? tag-space)
    default-value
    (loop [associations (conj (vec tag-space) (first tag-space))] ;; conj does wrap
      (if (or (empty? (rest associations))
              (<= tag (ffirst associations)))
        (second (first associations))
        (recur (rest associations))))))

;;; the following code (and the definition of tagdo-semantics above) is from 
;;; eval_with_tagging_with_args.clj

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
    ;; these calls return [value tag-space steps-remaining]
    (if (<= step-limit 0)
      [:limit-exceeded tag-space step-limit]
      (let [step-limit (dec step-limit)
            constants (merge (zipmap '(arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)
                                     (repeat default-value)) 
                             constants)]
        (if (not (seq? expression))
          [(get constants expression expression) tag-space step-limit]
          (if (= 1 (count expression))
            (if (map? (first expression))
              (eval-with-tagging
                (closest-association (:tagged (first expression)) tag-space default-value)
                tag-space step-limit constants default-value)
              [((resolve (first expression))) tag-space step-limit])
            (if (map? (first expression))
              (if (:tag (first expression))
                (if @tagdo-semantics
                  (eval-with-tagging (second expression)
                                     (assoc tag-space (:tag (first expression)) (second expression))
                                     step-limit
                                     constants
                                     default-value)
                  [default-value 
                   (assoc tag-space (:tag (first expression)) (second expression)) 
                   step-limit])
                ;; must be tagged-with-args 
                (eval-with-tagging
                  (clojure.walk/postwalk-replace
                    (zipmap '(arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)
                            (rest expression))
                    (closest-association (:tagged-with-args (first expression)) tag-space default-value))
                  tag-space step-limit constants default-value))
              (if (= 'if (first expression))
                (let [condition-eval-result 
                      (eval-with-tagging (second expression) tag-space step-limit constants default-value)]
                  (if (first condition-eval-result)
                    (eval-with-tagging (nth expression 2) 
                                       (nth condition-eval-result 1)
                                       (nth condition-eval-result 2)
                                       constants
                                       default-value)
                    (eval-with-tagging (nth expression 3) 
                                       (nth condition-eval-result 1)
                                       (nth condition-eval-result 2)
                                       constants
                                       default-value)))
                (let [arg-evaluation-results 
                      (loop [remaining (rest expression)
                             ts tag-space
                             lim step-limit
                             results []]
                        (if (empty? remaining)
                          results
                          (if (<= lim 0)
                            (recur (rest remaining) ts lim (conj results [:limit-exceeded ts lim]))
                            (let [first-result (eval-with-tagging 
                                                 (first remaining) ts lim constants default-value)]
                              (recur (rest remaining)
                                     (nth first-result 1)
                                     (nth first-result 2)
                                     (conj results first-result))))))
                      vals (map first arg-evaluation-results)
                      ending-limit (nth (last arg-evaluation-results) 2)
                      ending-ts (nth (last arg-evaluation-results) 1)]
                  (if (<= ending-limit 0)
                    [:limit-exceeded ending-ts ending-limit]
                    [(apply (resolve (first expression)) vals) ending-ts ending-limit]))))))))))
 
;;; end of code from eval_with_tagging_with_args.clj

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

;; random code generator from the GP field guide p 14

(defn update-terminal-proportion
  []
  (reset! terminal-proportion
          (/ (count @terminal-set)
             (+ (count @terminal-set) (count (keys @function-table))))))

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

(defn float-zero?
  "Tests if a float is close enough to zero to count as zero for evolution."
  [x]
  (and (> x (- @epsilon-for-float-equality))
       (< x @epsilon-for-float-equality)))

(defn pd
  "Protected biginteger or float division; returns 1 if the denominator is zero."
  [num denom]
  (if @integer-regression
    (if (zero? denom)
      1
      (bigint (quot num denom)))
    (if (float-zero? denom)
      1.0
      (float (/ num denom)))))
  
(defn abs ;; works even for bigints
  [x]
  (if (< x 0) (- x) x))

(defn ramp-depth [] (rand-nth (apply range @ramp-depth-range)))

(defn codesize [c]
  (if (seq? c)
    (count (flatten c))
    1))

(defn at-index 
  "Returns a subtree of tree indexed by point-index in a depth first traversal."
  [tree point-index]
  (let [index (mod (Math/abs point-index) (codesize tree))
        zipper (zip/seq-zip tree)]
    (loop [z zipper i index]
      (if (zero? i)
        (zip/node z)
        (if (seq? (zip/node z)) 
          (recur (zip/next (zip/next z)) (dec i))
          (recur (zip/next z) (dec i)))))))

(defn insert-at-index
  "Returns a copy of tree with the subtree formerly indexed by
   point-index (in a depth-first traversal) replaced by new-subtree."
  [tree point-index new-subtree]
  (let [index (mod (Math/abs point-index) (codesize tree))
        zipper (zip/seq-zip tree)]
    (loop [z zipper i index]
      (if (zero? i)
        (zip/root (zip/replace z new-subtree))
        (if (seq? (zip/node z))
          (recur (zip/next (zip/next z)) (dec i))
          (recur (zip/next z) (dec i)))))))

(defn annotate-points
  "Returns a sequence of [index kind] pairs where kind is :internal or :leaf."
  [tree]
  (let [limit (codesize tree)]
    (loop [z (zip/seq-zip tree) index 0 results []]
      (if (= index limit)
        results
        (if (seq? (zip/node z))
          (recur (zip/next (zip/next z))
                 (inc index) 
                 (conj results [index :internal]))
          (recur (zip/next z)
                 (inc index) 
                 (conj results [index :leaf])))))))

(defn select-node-90-10
  "returns an index"
  [tree]
  (let [annotated (annotate-points tree)
        internals (map first (filter #(= (second %) :internal) annotated))
        leaves (map first (filter #(= (second %) :leaf) annotated))]
    (cond (empty? internals) (rand-nth leaves)
          (empty? leaves)    (rand-nth internals)
          (< (rand) 0.9)     (rand-nth internals)
          :else              (rand-nth leaves))))

(defn select-node-by-tournament
  "returns an index"
  [tree]
  (let [num-nodes (codesize tree)
        tournament-set (repeatedly @node-tournament-size #(rand-int num-nodes))]
    (ffirst (sort #(> (first %1) (first %2))
                  (map #(vector % (codesize (at-index tree %))) tournament-set)))))
    
(defn select-node
  [tree]
  (case @node-selection-method
    :tournament (select-node-by-tournament tree)
    :koza (select-node-90-10 tree)
    ))

(defn depth
  [expression]
  (if (not (seq? expression))
    0
    (let [subsequences (filter seq? expression)]
      (if (empty? subsequences)
        1
        (inc (apply max (map depth subsequences)))))))

(defn mutate
  [i]
  (let [child (insert-at-index 
                i 
                (select-node i)
                (random-code (ramp-depth) (if (< (rand) 0.5) :grow :full)))]
    (if (> (depth child) @absolute-depth-limit)
      i
      child)))

(defn crossover
  [i j]
  (let [child (insert-at-index 
                i 
                (select-node i)
                (at-index j (select-node j)))]
    (if (> (depth child) @absolute-depth-limit)
      i
      child)))

(defn pmapall
  "Like pmap but: 1) coll should be finite, 2) the returned sequence
   will not be lazy, 3) calls to f may occur in any order, to maximize
   multicore processor utilization, and 4) takes only one coll so far."
  [f coll]
  (if @single-thread-mode
    (map f coll)
    (let [agents (map #(agent % :error-handler (fn [agnt except] (println except))) coll)]
      (dorun (map #(send % f) agents))
      (apply await agents)
      (doall (map deref agents)))))

;; During evolution we'll maintain the population as a sequence of
;; [program error] pairs.

(defn pair-with-errors
  "Returns a vector of [program error] pairs."
  [programs]
  (println "Computing errors...")
  (vec (pmapall #(vector % (@error-fn %)) programs)))

(defn select
  "Returns the program of the best [program error] pair in a tournament
   set with the specified size, location, and radius."
  [prog-err-pairs tournament-size location radius]
  (let [limit (count prog-err-pairs)
        tournament-set (repeatedly 
                         tournament-size 
                         #(nth prog-err-pairs
                               (mod (+ location 
                                       (- (rand-int (inc (* radius 2))) 
                                          radius))
                                    limit)))]
    (first (first (sort #(< (second %1) (second %2)) (vec tournament-set))))))

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

