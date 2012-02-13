(ns taggp.globals)

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
(def random-seed (atom (rand-int Integer/MAX_VALUE)))