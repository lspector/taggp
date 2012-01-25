;; Example file for taggp
;; Based on https://github.com/lspector/Clojush/src/examples/lawnmower.clj
;; Modifications by Kyle Harrington (kyleh@cs.brandeis.edu), 2012.
(ns taggp.examples.lawnmower
  (:use [taggp.core :exclude [-main]]))			      

(in-ns 'taggp.core)

(def lawn-size (atom {:x 8 :y 8}))
(defn expand-erc
  [item]
  (cond (= item :int-erc) (+ (first int-erc-range) 
                             (rand-int (inc (- (second int-erc-range)
                                               (first int-erc-range)))))
        (= item :float-erc) (+ (first int-erc-range) 
                               (* (rand)
                                  (- (second int-erc-range)
                                     (first int-erc-range))))
	(= item :intvec2D-erc) {:x (rand-int (:x @lawn-size))
				:y (rand-int (:y @lawn-size))}
        :else item))

(in-ns 'taggp.examples.lawnmower)
(use '[taggp.core :exclude [-main]])

(defn make-lawn
  [max-size limit]
  {:lawn-size max-size
   :current-pos {:x 0 :y 0}
   :orientation :east
   :mowed #{}
   :turns 0
   :moves 0
   :turn-limit limit
   :move-limit limit})

(def ^:dynamic *lawn* nil #_(make-lawn [8 8] 100))

(defn loc-ahead
  "Return the location one step forward."
  []
  {:x (mod (case (:orientation *lawn*)
		 :north (inc (:x (:current-pos *lawn*)))
		 :south (dec (:x (:current-pos *lawn*)))
		 (:x (:current-pos *lawn*)))
	   (:x (:lawn-size *lawn*)))
   :y (mod (case (:orientation *lawn*)
		 :east (inc (:y (:current-pos *lawn*)))
		 :west (dec (:y (:current-pos *lawn*)))
		 (:y (:current-pos *lawn*)))
	   (:y (:lawn-size *lawn*)))})

(defn mow
  "Mow one square of *lawn*."
  []
  (when (and (< (:turns *lawn*) (:turn-limit *lawn*))
	     (< (:moves *lawn*) (:move-limit *lawn*)))  
    (set! *lawn*
	  (assoc *lawn*
	    :current-pos (loc-ahead)
	    :mowed (conj (:mowed *lawn*)
			 (loc-ahead))	  
	    :moves (inc (:moves *lawn*)))))
  (:current-pos *lawn*))

(defn progn
  "Link two instructions. Return the result of b."
  [a b]
  b)

(defn left
  "Rotate orientation to the left."
  []
  (when (and (< (:turns *lawn*) (:turn-limit *lawn*))
	     (< (:moves *lawn*) (:move-limit *lawn*)))  
    (set! *lawn*
	  (assoc *lawn*
	    :orientation (get {:east :north,
			       :north :west,
			       :west :south,
			       :south :east}
			      (:orientation *lawn*))
	    :turns (inc (:turns *lawn*)))))
  {:x 0 :y 0})

(defn v8a
  "Add two 2D vectors mod lawn size. NOTE: Koza uses mod 8 all around."
  [a b]
  {:x (mod (+ (:x a) (:x b)) (:x (:lawn-size *lawn*)))
   :y (mod (+ (:y a) (:y b)) (:y (:lawn-size *lawn*)))})

(defn noop0 [] {:x 0, :y 0})
  
(defn noop1 [a] a)

(defn frog
  "Teleport to a given position."
  [a]
  (when (and (< (:turns *lawn*) (:turn-limit *lawn*))
	     (< (:moves *lawn*) (:move-limit *lawn*)))
    (set! *lawn*
	  (assoc *lawn*
	    :current-pos (v8a a (:current-pos *lawn*))
	    :mowed (conj (:mowed *lawn*)
			 a)
	    :moves (inc (:moves *lawn*)))))
  (:current-pos *lawn*))

(defn lawnmower-error
  "Error for the *lawn*mower problem."
  [size limit individual]
  (binding [*lawn* (make-lawn size limit)]
    (let [result (eval-with-tagging individual @execution-limit {} {:x 0 :y 0})])
    (- (* (:x size) (:y size))
       (count (:mowed *lawn*)))))

(defn run
  "An evolutionary run. This is separated for REPL usage. Size should be something
   like {:x 8 :y 8}."
  [size limit] 
  (reset! lawn-size size)
  (reset! function-table
          (let [basic-set (zipmap '(v8a frog progn mow left)
                                  '(2   1    2     0   0))]
            (if @allow-tagging
              (if @use-tag-with-args
                (merge basic-set
                       (zipmap '(:tagged-erf :tag-erf :tagged-with-args-erf)
                               '(0           1         1)))
                (merge basic-set
                       (zipmap '(:tagged-erf :tag-erf)
                               '(0           1))))
              (if @use-noops
                (merge basic-set
                       (zipmap '(noop0 noop1)
                               '(0     1)))
                basic-set))))
  (reset! terminal-set
          (let [basic-terminals '(:intvec2D-erc)]	    
            (if @allow-tagging
              (cons 'arg0 basic-terminals)
              basic-terminals)))
  (reset! error-fn (partial lawnmower-error size limit))
  (reset! successful-individual?
          (fn [individual]
            (zero? (second individual))))
  (update-terminal-proportion)
  (evolve))

(defn -main 
  [& params]
  (in-ns 'taggp.examples.lawnmower) ;; when using lein run (= *ns* 'user) by default, we need to switch
  (let [params (merge {:allow-tagging true
                       :tagdo-semantics true
                       :use-noops true
                       :lawn-width 8
                       :lawn-height 8
                       :action-limit 100};; both (move-limit and turn-limit) = action-limt
                      (apply hash-map (map read-string params)))
        lawn-size {:x (:lawn-width params)
                   :y (:lawn-height params)}]
    (println "lawn-size =" lawn-size)
    (println "action-limit =" (:action-limit params))
    (reset! allow-tagging (:allow-tagging params))
    (reset! tagdo-semantics (:tagdo-semantics params))
    (reset! use-noops (:use-noops params))
    (run lawn-size (:action-limit params))
    (System/exit 0)))
