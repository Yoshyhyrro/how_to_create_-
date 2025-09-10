;; src/p_adic_legacy_rescue/core.clj

(ns p-adic-legacy-rescue.core
  "Provides functions to analyze legacy code structures using ultrametric distances
  inspired by p-adic metrics. This implementation focuses on discovering naming
  patterns in COBOL variable names."
  (:require
    [clojure.string :as str]
    [cats.core :as m]
    [cats.monad.state :as state]
    [clojure.pprint :refer [pprint]]))


;;;-----------------------------------------------------------------------------
;;; Core Analysis Functions
;;;-----------------------------------------------------------------------------

(defn tokenize-name
  "Splits a COBOL-style variable name into a vector of string tokens."
  ^clojure.lang.IPersistentVector [^String s]
  (str/split s #"[.-_]"))

(defn common-prefix-length
  "Calculates the number of common leading tokens between two token vectors."
  ^long [^clojure.lang.IPersistentVector v1 ^clojure.lang.IPersistentVector v2]
  (->> (map vector v1 v2)
       (take-while (fn [[x y]] (= x y)))
       count))

(defn ultrametric-distance
  "Calculates an ultrametric distance based on common prefix length."
  ^double [^clojure.lang.IPersistentVector base-tokens
            ^clojure.lang.IPersistentVector other-tokens
            ^long p]
  (let [prefix-len (common-prefix-length base-tokens other-tokens)]
    (/ 1.0 (Math/pow p (inc prefix-len)))))

(defn group-by-prefix-hierarchy
  "Groups a list of variable names into a hierarchy based on their ultrametric
  distance from a given base variable."
  [base-var var-names p]
  (let [base-tokens (tokenize-name base-var)]
    (->> var-names
         (map (fn [var-name] {:name var-name :tokens (tokenize-name var-name)}))
         (group-by (fn [item] (common-prefix-length base-tokens (:tokens item))))
         (sort-by key >)
         (map (fn [[depth items]]
                {:depth depth
                 :distance (/ 1.0 (Math/pow p (inc depth)))
                 :members (map :name items)
                 :count (count items)})))))


;;;-----------------------------------------------------------------------------
;;; Advanced & Parallel Execution
;;;-----------------------------------------------------------------------------

;; Helper function to modify state, as `state/modify` is not always available.
;; This is the robust solution you correctly pointed out.
(defn modify [f]
  (state/state (fn [s] [nil (f s)])))

(defn stateful-analysis
  "Wraps the analysis in a State monad to track progress metadata."
  [base-var variables p]
  (state/run
    (m/mlet [_        (modify #(update % :processed-vars (fnil + 0) (count variables)))
             clusters (m/return (group-by-prefix-hierarchy base-var variables p))
             _        (modify #(assoc % :cluster-count (count clusters)))]
      (m/return clusters))
    {:processed-vars 0 :cluster-count 0}))

(defn parallel-cobol-analysis
  "Analyzes multiple base variables against a collection of all variables in parallel."
  [base-vars all-variables p]
  (->> base-vars
       (pmap #(vector % (group-by-prefix-hierarchy % all-variables p)))
       (into {})))


;;;-----------------------------------------------------------------------------
;;; Example Data and Main Execution
;;;-----------------------------------------------------------------------------

(def cobol-variables
  "A sample list of COBOL variable names for demonstration."
  ["WS-CUST-ID" "WS-CUST-NAME" "WS-CUST-ADDR" "WS-CUST-PHONE"
   "WS-ORDER-ID" "WS-ORDER-DATE" "WS-ORDER-TOTAL"
   "PRINT-HEADER" "PRINT-DETAIL" "PRINT-FOOTER"
   "DB-CONNECT" "DB-CURSOR" "FILE-INPUT" "FILE-OUTPUT"])

(def base-patterns
  "A sample list of prefixes to drive the parallel analysis."
  ["WS-CUST" "WS-ORDER" "PRINT" "DB" "FILE"])


(defn -main
  "Application entry point. Runs a series of demonstrations."
  [& args]
  (println "=====================================================")
  (println "=== p-adic Inspired COBOL Variable Name Analysis ===")
  (println "=====================================================")

  (println "\n--- 1. Single Base Variable Analysis ('WS-CUST-ID') ---")
  (pprint (group-by-prefix-hierarchy "WS-CUST-ID" cobol-variables 2))

  (println "\n--- 2. Parallel Analysis of Multiple Base Patterns ---")
  (pprint (parallel-cobol-analysis base-patterns cobol-variables 2))

  (println "\n--- 3. Stateful Analysis Example ---")
  (let [[result final-state] (stateful-analysis "WS-CUST-ID" cobol-variables 2)]
    (println "Final State:" final-state)
    (println "Result:")
    (pprint result))

  (println "\nAnalysis complete."))

;; REPL-friendly entry points
(comment
  (group-by-prefix-hierarchy "WS-CUST-ID" cobol-variables 2)
  (parallel-cobol-analysis base-patterns cobol-variables 2)
  (stateful-analysis "WS-CUST-ID" cobol-variables 2))
