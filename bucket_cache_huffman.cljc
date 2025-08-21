(ns huffman-bucket-cache
  "Huffman coding for efficient bucket cache lookups"
  (:import [java.util PriorityQueue Comparator]))

;; --- Huffman Tree Construction ---

(defrecord HuffmanNode [freq value left right code])

(defn create-huffman-comparator []
  (reify Comparator
    (compare [_ a b]
      (Integer/compare (:freq a) (:freq b)))))

(defn build-huffman-tree
  "Build Huffman tree from bucket frequency map"
  [freq-map]
  (let [queue (PriorityQueue. (create-huffman-comparator))]
    
    ;; Initialize leaf nodes
    (doseq [[bucket freq] freq-map]
      (.add queue (->HuffmanNode freq bucket nil nil nil)))
    
    ;; Build tree bottom-up
    (while (> (.size queue) 1)
      (let [left (.poll queue)
            right (.poll queue)
            merged (->HuffmanNode (+ (:freq left) (:freq right))
                                 nil left right nil)]
        (.add queue merged)))
    
    (.poll queue)))

(defn assign-huffman-codes
  "Assign binary codes to each bucket based on tree structure"
  [root]
  (let [codes (atom {})]
    (letfn [(traverse [node code]
              (when node
                (if (:value node)  ; leaf node
                  (swap! codes assoc (:value node) code)
                  (do  ; internal node
                    (traverse (:left node) (str code "0"))
                    (traverse (:right node) (str code "1"))))))]
      (if (:value root)  ; Single node case
        (swap! codes assoc (:value root) "0")
        (traverse root ""))
      @codes)))

;; --- Bucket Cache with Huffman Lookup ---

(defn analyze-bucket-frequencies
  "Analyze access patterns to determine bucket frequencies"
  [access-log bucket-fn]
  (frequencies (map bucket-fn access-log)))

(defn create-huffman-lookup-table
  "Create optimized lookup table using Huffman coding"
  [bucket-frequencies]
  (let [huffman-tree (build-huffman-tree bucket-frequencies)
        huffman-codes (assign-huffman-codes huffman-tree)]
    
    ;; Create bidirectional lookup
    {:code-to-bucket huffman-codes
     :bucket-to-code (into {} (map (fn [[k v]] [v k]) huffman-codes))
     :tree huffman-tree
     :avg-bits (/ (reduce + (map (fn [[bucket freq]]
                                   (* freq (count (huffman-codes bucket))))
                                 bucket-frequencies))
                  (reduce + (vals bucket-frequencies)))}))

(defn compress-bucket-sequence
  "Compress sequence of bucket accesses using Huffman codes"
  [bucket-sequence huffman-table]
  (let [codes (:code-to-bucket huffman-table)]
    (apply str (map codes bucket-sequence))))

(defn decode-huffman-sequence
  "Decode compressed bucket sequence back to bucket IDs"
  [compressed-bits huffman-tree]
  (loop [bits compressed-bits
         current-node huffman-tree
         result []]
    (if (empty? bits)
      (if (:value current-node)  ; Handle final partial code
        (conj result (:value current-node))
        result)
      (let [bit (first bits)
            next-node (if (= bit \0) (:left current-node) (:right current-node))]
        (if (:value next-node)  ; Reached leaf
          (recur (rest bits) huffman-tree (conj result (:value next-node)))
          (recur (rest bits) next-node result))))))

;; --- Cache Implementation with Huffman Optimization ---

(defrecord HuffmanBucketCache [buckets huffman-table access-log])

(defn create-huffman-bucket-cache
  "Create bucket cache with Huffman-optimized lookup"
  [initial-buckets access-patterns bucket-fn]
  (let [frequencies (analyze-bucket-frequencies access-patterns bucket-fn)
        huffman-table (create-huffman-lookup-table frequencies)]
    
    (println "=== Huffman Optimization Results ===")
    (println (format "Average bits per lookup: %.2f" (:avg-bits huffman-table)))
    (println "Huffman codes:")
    (doseq [[bucket code] (:code-to-bucket huffman-table)]
      (let [freq (frequencies bucket 0)]
        (println (format "  Bucket %s: '%s' (freq: %d, bits: %d)"
                         bucket code freq (count code)))))
    
    (->HuffmanBucketCache initial-buckets huffman-table [])))

(defn huffman-bucket-lookup
  "Optimized bucket lookup using Huffman codes"
  [cache bucket-id]
  (let [huffman-code (get-in cache [:huffman-table :code-to-bucket bucket-id])
        bucket-data (get-in cache [:buckets bucket-id])]
    
    ;; Simulate compressed lookup (in practice, this would use bit operations)
    {:bucket-id bucket-id
     :huffman-code huffman-code
     :compression-bits (count huffman-code)
     :data bucket-data}))

;; --- Practical Example: Distance-based Bucket Cache ---

(def sample-nodes
  [{:id 1 :dist 5}   {:id 2 :dist 12}  {:id 3 :dist 7}
   {:id 4 :dist 128} {:id 5 :dist 99}  {:id 6 :dist 256}
   {:id 7 :dist 1}   {:id 8 :dist 1024} {:id 9 :dist 500}
   {:id 10 :dist 15} {:id 11 :dist 25}  {:id 12 :dist 50}])

(defn bit-bucket-fn [shift-level]
  (fn [node] (bit-shift-right (:dist node) shift-level)))

;; Simulate access patterns (some buckets accessed more frequently)
(def access-patterns
  (concat
    ;; Frequent access to near distances (bucket 0)
    (repeat 100 {:dist 10})
    (repeat 80 {:dist 25})
    (repeat 90 {:dist 50})
    ;; Less frequent access to medium distances
    (repeat 20 {:dist 300})
    (repeat 15 {:dist 500})
    ;; Rare access to far distances
    (repeat 5 {:dist 1000})
    (repeat 3 {:dist 2000})))

;; --- Performance Analysis ---

(defn analyze-compression-efficiency
  "Compare Huffman coding vs uniform coding"
  [bucket-frequencies huffman-table]
  (let [total-accesses (reduce + (vals bucket-frequencies))
        uniform-bits (* total-accesses (int (Math/ceil (Math/log2 (count bucket-frequencies)))))
        huffman-bits (int (* total-accesses (:avg-bits huffman-table)))
        compression-ratio (double (/ huffman-bits uniform-bits))]
    
    (println "\n=== Compression Analysis ===")
    (println (format "Total bucket accesses: %d" total-accesses))
    (println (format "Uniform coding: %d bits (%.1f bits/lookup)"
                     uniform-bits (double (/ uniform-bits total-accesses))))
    (println (format "Huffman coding: %d bits (%.2f bits/lookup)"
                     huffman-bits (:avg-bits huffman-table)))
    (println (format "Compression ratio: %.2f (%.1f%% savings)"
                     compression-ratio (* (- 1 compression-ratio) 100)))
    
    {:uniform-bits uniform-bits
     :huffman-bits huffman-bits
     :compression-ratio compression-ratio
     :savings-percent (* (- 1 compression-ratio) 100)}))

;; --- Demonstration ---

(defn demonstrate-huffman-bucket-cache []
  (println "=== Huffman-Coded Bucket Cache Demo ===")
  
  ;; Create initial buckets using 8-bit bucketing
  (let [bucket-fn (bit-bucket-fn 8)
        initial-buckets (->> sample-nodes
                            (group-by bucket-fn)
                            (into {}))
        
        ;; Create Huffman-optimized cache
        huffman-cache (create-huffman-bucket-cache 
                        initial-buckets access-patterns bucket-fn)
        
        ;; Analyze compression efficiency
        bucket-frequencies (analyze-bucket-frequencies access-patterns bucket-fn)]
    
    (analyze-compression-efficiency bucket-frequencies (:huffman-table huffman-cache))
    
    (println "\n=== Lookup Examples ===")
    (doseq [bucket-id (keys initial-buckets)]
      (let [lookup-result (huffman-bucket-lookup huffman-cache bucket-id)]
        (println (format "Bucket %d: code '%s' (%d bits) -> %d nodes"
                         bucket-id (:huffman-code lookup-result)
                         (:compression-bits lookup-result)
                         (count (:data lookup-result))))))
    
    ;; Demonstrate sequence compression
    (let [test-sequence [0 0 0 1 0 4 0 0 8]  ; Simulated bucket access sequence
          compressed (compress-bucket-sequence 
                       test-sequence (:huffman-table huffman-cache))
          decoded (decode-huffman-sequence 
                    compressed (:tree (:huffman-table huffman-cache)))]
      
      (println (format "\nSequence compression demo:"))
      (println (format "  Original: %s" (vec test-sequence)))
      (println (format "  Compressed: '%s' (%d bits)" compressed (count compressed)))
      (println (format "  Decoded: %s" (vec decoded)))
      (println (format "  Compression: %d -> %d bits (%.1f%%)"
                       (* (count test-sequence) 4)  ; Assume 4 bits uniform
                       (count compressed)
                       (* 100.0 (/ (count compressed) (* (count test-sequence) 4))))))))

;; Execute demonstration
(demonstrate-huffman-bucket-cache)
