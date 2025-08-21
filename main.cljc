(ns bucket-sort-priority-queue
  "Bucket sort with java.util.PriorityQueue - practical Clojure approach"
  (:import [java.util PriorityQueue Comparator]))

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

;; --- Core bucket sort with PriorityQueue ---

(defn create-distance-comparator
  "Create a comparator for sorting by distance"
  []
  (reify Comparator
    (compare [_ a b]
      (Integer/compare (:dist a) (:dist b)))))

(defn bucketize-with-priority-queues
  "Bucket sort using PriorityQueue for each bucket
   Returns map of bucket-key -> sorted PriorityQueue"
  [nodes bucket-fn]
  (let [buckets (transient {})]
    (doseq [node nodes]
      (let [bucket-key (bucket-fn node)
            queue (get buckets bucket-key
                      (PriorityQueue. (create-distance-comparator)))]
        (.add queue node)
        (assoc! buckets bucket-key queue)))
    (persistent! buckets)))

;; --- Bucket functions (same as before but adapted) ---

(defn bit-bucket-fn
  "Bucket function using bit shifting"
  [shift-level]
  (fn [node] (bit-shift-right (:dist node) shift-level)))

(defn digit-bucket-fn
  "Bucket function using digit count"
  []
  (fn [node]
    (let [dist (:dist node)]
      (if (zero? dist) 1
          (inc (int (Math/log10 dist)))))))

(defn range-bucket-fn
  "Bucket function using fixed ranges"
  [range-size]
  (fn [node] (quot (:dist node) range-size)))

;; --- Utility functions for PriorityQueue manipulation ---

(defn priority-queue-to-seq
  "Convert PriorityQueue to sorted sequence (destructive operation)"
  [pq]
  (loop [result []]
    (if-let [item (.poll pq)]
      (recur (conj result item))
      result)))

(defn peek-queue
  "Non-destructively peek at queue contents"
  [pq]
  (vec (.toArray pq)))

(defn merge-sorted-buckets
  "Merge all buckets maintaining sort order using PriorityQueues"
  [bucket-map]
  (let [master-queue (PriorityQueue. (create-distance-comparator))]
    ;; Add all elements from all buckets to master queue
    (doseq [[_ queue] bucket-map]
      (doseq [item (peek-queue queue)]
        (.add master-queue item)))
    (priority-queue-to-seq master-queue)))

;; --- Analysis and demonstration functions ---

(defn analyze-bucket-performance
  "Analyze bucket distribution and performance"
  [bucket-map]
  (let [bucket-sizes (map #(.size %) (vals bucket-map))
        total-items (reduce + bucket-sizes)]
    {:total-items total-items
     :bucket-count (count bucket-map)
     :avg-size (if (pos? total-items) (double (/ total-items (count bucket-map))) 0)
     :max-size (if (empty? bucket-sizes) 0 (apply max bucket-sizes))
     :min-size (if (empty? bucket-sizes) 0 (apply min bucket-sizes))
     :bucket-sizes bucket-sizes}))

(defn demonstrate-priority-queue-bucketing
  "Show different bucketing strategies with PriorityQueues"
  []
  (println "=== 元データ ===")
  (doseq [node nodes]
    (println (format "ID: %d, Distance: %d" (:id node) (:dist node))))
  
  (println "\n=== 8ビットバケッティング (PriorityQueue版) ===")
  (let [buckets (bucketize-with-priority-queues nodes (bit-bucket-fn 8))
        stats (analyze-bucket-performance buckets)]
    
    (doseq [[bucket-key queue] (sort-by key buckets)]
      (let [range-start (* bucket-key 256)
            range-end (+ range-start 255)
            items (peek-queue queue)]
        (println (format "バケット %d [%d-%d]: %d個"
                         bucket-key range-start range-end (.size queue)))
        (println (format "  内容: %s"
                         (clojure.string/join ", " 
                           (map #(format "ID:%d(dist:%d)" (:id %) (:dist %)) items))))))
    
    (println (format "\n統計: %s" stats)))
  
  (println "\n=== 桁数バケッティング (PriorityQueue版) ===")
  (let [buckets (bucketize-with-priority-queues nodes (digit-bucket-fn))
        stats (analyze-bucket-performance buckets)]
    
    (doseq [[digits queue] (sort-by key buckets)]
      (let [items (peek-queue queue)]
        (println (format "%d桁の数値: %d個"
                         digits (.size queue)))
        (println (format "  内容: %s"
                         (clojure.string/join ", "
                           (map #(format "ID:%d(dist:%d)" (:id %) (:dist %)) items))))))
    
    (println (format "\n統計: %s" stats))))

;; --- Advanced: Bucket sort with k-way merge ---

(defn k-way-merge-bucket-sort
  "Full bucket sort implementation with k-way merge"
  [nodes bucket-fn]
  (let [buckets (bucketize-with-priority-queues nodes bucket-fn)]
    (println (format "バケット数: %d" (count buckets)))
    (merge-sorted-buckets buckets)))

;; --- Performance comparison ---

(defn compare-approaches
  "Compare different approaches on larger dataset"
  [data-size max-value]
  (let [large-dataset (for [i (range data-size)]
                        {:id i :dist (rand-int max-value)})
        
        approaches {"8ビットバケット" (bit-bucket-fn 8)
                    "桁数バケット" (digit-bucket-fn)
                    "範囲バケット(1000)" (range-bucket-fn 1000)}]
    
    (println (format "\n=== パフォーマンス比較 (n=%d, max=%d) ===" 
                     data-size max-value))
    
    (doseq [[name bucket-fn] approaches]
      (let [start-time (System/nanoTime)
            buckets (bucketize-with-priority-queues large-dataset bucket-fn)
            end-time (System/nanoTime)
            duration (/ (- end-time start-time) 1000000.0)
            stats (analyze-bucket-performance buckets)]
        
        (println (format "%s: %.2f ms, %d buckets, 最大 %d個, 平均 %.1f個"
                         name duration (:bucket-count stats) 
                         (:max-size stats) (:avg-size stats)))))))

;; --- Practical example: Finding k nearest neighbors ---

(defn find-k-nearest-with-buckets
  "Find k nearest neighbors using bucket optimization"
  [nodes target-dist k bucket-fn]
  (let [buckets (bucketize-with-priority-queues nodes bucket-fn)
        target-bucket (bucket-fn {:dist target-dist})
        
        ;; Check target bucket and adjacent buckets
        relevant-buckets (filter some? 
                                 (map #(get buckets %) 
                                      [(dec target-bucket) target-bucket (inc target-bucket)]))
        
        ;; Merge relevant buckets
        candidates (PriorityQueue. 
                     (reify Comparator
                       (compare [_ a b]
                         (Integer/compare 
                           (Math/abs (- (:dist a) target-dist))
                           (Math/abs (- (:dist b) target-dist))))))]
    
    ;; Add candidates from relevant buckets
    (doseq [bucket relevant-buckets]
      (doseq [node (peek-queue bucket)]
        (.add candidates node)))
    
    ;; Extract k nearest
    (take k (priority-queue-to-seq candidates))))

;; Execute demonstrations
(demonstrate-priority-queue-bucketing)

;; Test k-nearest neighbor search
(println "\n=== k近傍探索テスト ===")
(let [target 100
      k 3
      nearest (find-k-nearest-with-buckets nodes target k (bit-bucket-fn 8))]
  (println (format "距離 %d に最も近い %d個のノード:" target k))
  (doseq [node nearest]
    (println (format "  ID: %d, Distance: %d, 差分: %d"
                     (:id node) (:dist node) (Math/abs (- (:dist node) target))))))

(compare-approaches 1000 10000)
