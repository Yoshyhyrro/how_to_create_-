(ns bucket-sort-core
  "Bucket sort implementations using clojure.core functions")

;; Sample data
(def nodes
  [{:id 1 :dist 5}
   {:id 2 :dist 12}
   {:id 3 :dist 7}
   {:id 4 :dist 128}
   {:id 5 :dist 99}
   {:id 6 :dist 256}
   {:id 7 :dist 1}
   {:id 8 :dist 1024}])

;; --- Core Implementation 1: Bit-based bucketing ---
;; Uses mathematical properties of binary representation
;; Time complexity: O(n), Space complexity: O(n + k) where k is number of buckets

(defn bucketize-by-bits
  "Bucket nodes by upper bits of distance values
   Uses bit-shift-right to extract higher-order bits as bucket keys"
  [nodes shift-level]
  (->> nodes
       (group-by #(bit-shift-right (:dist %) shift-level))
       (into (sorted-map))))

;; Enhanced version with configurable extraction function
(defn bucketize-by-function
  "Generic bucketing using any key extraction function"
  [nodes key-fn]
  (->> nodes
       (group-by key-fn)
       (into (sorted-map))))

;; --- Core Implementation 2: Decimal digit-based bucketing ---
;; Uses logarithmic properties for digit counting
;; More intuitive for human understanding

(defn digit-count
  "Calculate number of digits in a positive integer
   Uses logarithmic approach for efficiency"
  [n]
  (if (zero? n) 1
      (inc (int (Math/log10 n)))))

(defn bucketize-by-digits
  "Bucket nodes by number of decimal digits in distance"
  [nodes]
  (->> nodes
       (group-by #(digit-count (:dist %)))
       (into (sorted-map))))

;; --- Alternative implementations using different core functions ---

(defn bucketize-by-range
  "Bucket nodes into fixed-size ranges
   Uses integer division for uniform distribution"
  [nodes range-size]
  (->> nodes
       (group-by #(quot (:dist %) range-size))
       (into (sorted-map))))

(defn bucketize-by-log-scale
  "Bucket nodes using logarithmic scale
   Natural for exponentially distributed data"
  [nodes base]
  (->> nodes
       (group-by #(if (zero? (:dist %)) 0
                      (int (/ (Math/log (:dist %))
                              (Math/log base)))))
       (into (sorted-map))))

;; --- Utility functions for analysis ---

(defn bucket-statistics
  "Analyze bucket distribution for load balancing"
  [buckets]
  (let [sizes (map count (vals buckets))
        total (reduce + sizes)]
    {:total-items total
     :bucket-count (count buckets)
     :avg-size (if (pos? total) (double (/ total (count buckets))) 0)
     :max-size (if (empty? sizes) 0 (apply max sizes))
     :min-size (if (empty? sizes) 0 (apply min sizes))
     :size-variance (if (> (count sizes) 1)
                      (let [mean (/ total (count buckets))
                            variance (/ (reduce + (map #(* (- % mean) (- % mean)) sizes))
                                        (dec (count sizes)))]
                        variance)
                      0)}))

(defn extract-ids
  "Extract just the IDs from bucketed results for cleaner output"
  [buckets]
  (into (sorted-map)
        (map (fn [[k v]] [k (map :id v)]))
        buckets))

;; --- Demonstration and comparison ---

(defn demonstrate-approaches []
  (println "=== Original Data ===")
  (doseq [node nodes]
    (println (format "ID: %d, Distance: %d (binary: %s)"
                     (:id node) (:dist node)
                     (Integer/toBinaryString (:dist node)))))
  
  (println "\n=== Bit-based Bucketing (shift level 3) ===")
  (let [bit-buckets (bucketize-by-bits nodes 3)]
    (doseq [[bucket items] (extract-ids bit-buckets)]
      (println (format "Bucket %d: %s" bucket items)))
    (println "Statistics:" (bucket-statistics bit-buckets)))
  
  (println "\n=== Digit-based Bucketing ===")
  (let [digit-buckets (bucketize-by-digits nodes)]
    (doseq [[digits items] (extract-ids digit-buckets)]
      (println (format "%d-digit numbers: %s" digits items)))
    (println "Statistics:" (bucket-statistics digit-buckets)))
  
  (println "\n=== Range-based Bucketing (range size 100) ===")
  (let [range-buckets (bucketize-by-range nodes 100)]
    (doseq [[range items] (extract-ids range-buckets)]
      (println (format "Range [%d-%d): %s" 
                       (* range 100) (+ (* range 100) 99) items)))
    (println "Statistics:" (bucket-statistics range-buckets)))
  
  (println "\n=== Log-scale Bucketing (base 2) ===")
  (let [log-buckets (bucketize-by-log-scale nodes 2)]
    (doseq [[power items] (extract-ids log-buckets)]
      (println (format "2^%d range: %s" power items)))
    (println "Statistics:" (bucket-statistics log-buckets))))

;; --- Performance comparison function ---

(defn compare-performance
  "Compare different bucketing strategies on larger datasets"
  [data-size max-value]
  (let [large-dataset (for [i (range data-size)]
                        {:id i :dist (rand-int max-value)})]
    (println (format "\n=== Performance Comparison (n=%d, max=%d) ===" 
                     data-size max-value))
    
    (doseq [[name f] [["Bit-shift (level 4)" #(bucketize-by-bits % 4)]
                      ["Digit-count" bucketize-by-digits]
                      ["Range (size 1000)" #(bucketize-by-range % 1000)]
                      ["Log-scale (base 10)" #(bucketize-by-log-scale % 10)]]]
      (let [start-time (System/nanoTime)
            result (f large-dataset)
            end-time (System/nanoTime)
            duration (/ (- end-time start-time) 1000000.0)]
        (println (format "%s: %.2f ms, %d buckets, %s"
                         name duration (count result)
                         (bucket-statistics result)))))))

;; --- Specific 8-bit analysis function ---

(defn analyze-8bit-bucketing
  "Analyze the effectiveness of 8-bit (÷256) bucketing strategy"
  [nodes]
  (let [buckets (bucketize-by-bits nodes 8)  ; 8-bit shift = ÷256
        bucket-sizes (map count (vals buckets))
        total-items (reduce + bucket-sizes)]
    
    (println "=== 8ビット（256単位）バケッティング分析 ===")
    
    ;; Show bucket distribution
    (doseq [[bucket-key items] buckets]
      (let [range-start (* bucket-key 256)
            range-end (+ range-start 255)
            distances (map :dist items)]
        (println (format "バケット %d [%d-%d]: %d個 - %s"
                         bucket-key range-start range-end
                         (count items) (str/join ", " distances)))))
    
    ;; Precision analysis
    (println "\n=== 精度分析 ===")
    (doseq [[bucket-key items] buckets]
      (when (> (count items) 1)
        (let [distances (map :dist items)
              max-dist (apply max distances)
              min-dist (apply min distances)
              max-error (- max-dist min-dist)]
          (println (format "バケット %d: 最大誤差 ±%d (範囲 %d-%d)"
                           bucket-key (/ max-error 2) min-dist max-dist)))))
    
    ;; Efficiency analysis
    (println "\n=== 効率分析 ===")
    (println (format "総要素数: %d" total-items))
    (println (format "バケット数: %d" (count buckets)))
    (println (format "削減率: %.1f%%" 
                     (* 100.0 (- 1.0 (/ (count buckets) total-items)))))
    (println (format "最大バケットサイズ: %d" 
                     (if (empty? bucket-sizes) 0 (apply max bucket-sizes))))
    (println (format "平均バケットサイズ: %.1f"
                     (if (pos? (count buckets)) 
                       (double (/ total-items (count buckets))) 0)))))

;; Test with practical distance data
(def practical-distances
  (map-indexed (fn [idx dist] {:id idx :dist dist})
               [1 3 5 8 12 15 23 45 67 89      ; 近距離 (0-255)
                256 300 400 500                 ; 中距離 (256-511)  
                600 800 1000                    ; 中距離続き
                1500 2000 2500                  ; 遠距離
                5000 8000 10000]))              ; 超遠距離

;; Execute analysis
(println "実践的な距離データでの8ビットバケッティング:")
(analyze-8bit-bucketing practical-distances)
