;; src/p_adic_legacy_rescue/core.clj

(ns p-adic-legacy-rescue.core
  "Provides functions to analyze legacy code structures using ultrametric distances
  inspired by p-adic metrics. This implementation focuses on discovering naming
  patterns in COBOL variable names."
  (:require
    [clojure.string :as str]
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

(defn p-adic-distance
  "Calculates p-adic ultrametric distance: closer prefixes = smaller distance"
  ^double [^clojure.lang.IPersistentVector base-tokens
            ^clojure.lang.IPersistentVector other-tokens
            ^long p]
  (let [prefix-len (common-prefix-length base-tokens other-tokens)]
    (/ 1.0 (Math/pow p (inc prefix-len)))))

(defn analyze-cobol-structure
  "Cluster COBOL variables by p-adic distance hierarchy"
  [base-var var-names p]
  (let [base-tokens (tokenize-name base-var)]
    (->> var-names
         (map #(vector % (tokenize-name %)))
         (group-by (fn [[_ tokens]] 
                     (common-prefix-length base-tokens tokens)))
         (sort-by first >)  ;; Sort by depth (deeper first)
         (map (fn [[depth items]]
                {:depth depth
                 :distance (/ 1.0 (Math/pow p (inc depth)))
                 :members (map first items)
                 :count (count items)})))))


;;;-----------------------------------------------------------------------------
;;; System-Level Architecture Discovery
;;;-----------------------------------------------------------------------------

(defn discover-system-hierarchy
  "Discover complete system structure by analyzing multiple base patterns"
  [all-variables base-patterns p]
  (->> base-patterns
       (pmap (fn [base-pattern]
               (let [matching-vars (filter #(str/starts-with? % base-pattern) 
                                          all-variables)]
                 (when (seq matching-vars)
                   {:pattern base-pattern
                    :subsystem-size (count matching-vars)
                    :internal-structure (analyze-cobol-structure 
                                        (first matching-vars) matching-vars p)}))))
       (remove nil?)
       (sort-by :subsystem-size >)))

(defn enterprise-cobol-analysis
  "Automatically discover base patterns and analyze at scale"
  [all-variables p threshold]
  (let [;; Extract potential base patterns from variable prefixes
        base-candidates (->> all-variables
                            (map tokenize-name)
                            (mapcat #(take 2 %))  ; Consider 1-2 token prefixes
                            frequencies
                            (filter #(>= (second %) threshold))  ; Min occurrence threshold
                            (map first))
        
        ;; Analyze each significant pattern
        analysis-results (discover-system-hierarchy all-variables base-candidates p)]
    
    {:total-variables (count all-variables)
     :base-patterns-found (count base-candidates)
     :major-subsystems (take 10 analysis-results)
     :coverage-ratio (/ (apply + (map :subsystem-size analysis-results))
                       (count all-variables))}))


;;;-----------------------------------------------------------------------------
;;; Parallel Analysis for Multiple Base Variables
;;;-----------------------------------------------------------------------------

(defn parallel-cobol-analysis
  "Analyzes multiple base variables against a collection of all variables in parallel."
  [base-vars all-variables p]
  (->> base-vars
       (pmap #(vector % (analyze-cobol-structure % all-variables p)))
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
  "Base patterns for system hierarchy discovery."
  ["WS-CUST" "WS-ACCT" "WS-ORDER" "DB-" "PRINT-" "ERR-"])

(def enterprise-sample-variables
  "Larger sample for enterprise analysis demonstration."
  (concat cobol-variables
          ["WS-ACCT-BALANCE" "WS-ACCT-TYPE" "WS-ACCT-STATUS"
           "ERR-MSG-TEXT" "ERR-CODE" "ERR-MODULE-ID"
           "DB-CUSTOMER-TBL-ID" "DB-CUSTOMER-TBL-NAME"
           "DB-TRANSACT-HST-ID" "DB-TRANSACT-HST-DATE"]))


(defn -main
  "Application entry point. Runs a series of demonstrations."
  [& args]
  (println "=====================================================")
  (println "=== p-adic Inspired COBOL Variable Name Analysis ===")
  (println "=====================================================")

  (println "\n--- 1. Single Base Variable Analysis ('WS-CUST-ID') ---")
  (pprint (analyze-cobol-structure "WS-CUST-ID" cobol-variables 2))

  (println "\n--- 2. Distance Calculation Examples ---")
  (let [base ["WS" "CUST" "ID"]
        vars [["WS" "CUST" "NAME"]    ;; prefix=2 → distance=1/8
              ["WS" "ORDER" "ID"]     ;; prefix=1 → distance=1/4  
              ["PRINT" "HEADER"]]]    ;; prefix=0 → distance=1/2
    (println "Distance calculations with p=2:")
    (doseq [[var-tokens expected] (map vector vars [0.125 0.25 0.5])]
      (let [distance (p-adic-distance base var-tokens 2)]
        (println (format "  %s -> %.3f (expected: %.3f)" 
                        var-tokens distance expected)))))

  (println "\n--- 3. System Hierarchy Discovery ---")
  (pprint (discover-system-hierarchy enterprise-sample-variables 
                                   ["WS-CUST" "WS-ACCT" "DB-" "ERR-"] 2))

  (println "\n--- 4. Enterprise-Scale Analysis ---")
  (pprint (enterprise-cobol-analysis enterprise-sample-variables 2 2))

  (println "\n--- 5. Parallel Analysis of Multiple Base Patterns ---")
  (pprint (parallel-cobol-analysis ["WS-CUST-ID" "PRINT-HEADER" "DB-CONNECT"] 
                                  cobol-variables 2))

  (println "\nAnalysis complete."))

;; REPL-friendly entry points
(comment
  ;; Basic analysis
  (analyze-cobol-structure "WS-CUST-ID" cobol-variables 2)
  
  ;; Distance examples from the article
  (let [base ["WS" "CUST" "ID"]]
    (map #(p-adic-distance base % 2) 
         [["WS" "CUST" "NAME"] ["WS" "ORDER" "ID"] ["PRINT" "HEADER"]]))
  
  ;; System hierarchy discovery
  (discover-system-hierarchy enterprise-sample-variables 
                           ["WS-CUST" "WS-ACCT" "DB-" "ERR-"] 2)
  
  ;; Enterprise analysis
  (enterprise-cobol-analysis enterprise-sample-variables 2 2)
  
  ;; Parallel analysis
  (parallel-cobol-analysis ["WS-CUST-ID" "PRINT-HEADER"] cobol-variables 2)
)
