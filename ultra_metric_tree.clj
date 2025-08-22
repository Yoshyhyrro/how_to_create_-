(ns ultra-metric-tree
  "関数型スタイルによるウルトラメトリック距離データ構造"
  (:require [clojure.data.priority-map :as pm]
            [clojure.string :as str])
  (:import [java.nio ByteBuffer]
           [java.util Arrays]))

;; --- プロトコル定義 ---

(defprotocol MetricSpace
  "メトリック空間の抽象化"
  (distance [this other] "2点間の距離を計算")
  (ball [this radius] "指定半径内の近傍を取得")
  (diameter [this] "空間の直径を取得"))

(defprotocol UltraMetricTree
  "ウルトラメトリック木の操作プロトコル"
  (um-insert [tree key value] "キー・値ペアを挿入")
  (um-search [tree key max-dist] "距離制限付き検索")
  (um-k-nearest [tree key k] "k近傍検索")
  (um-stats [tree] "構造統計情報"))

;; --- バイト変換ユーティリティ ---

(defn ^bytes to-byte-array
  "任意の値を統一的にバイト配列に変換"
  [value]
  (letfn [(string->bytes [^String s] (.getBytes s "UTF-8"))
          (number->bytes [n] (-> (ByteBuffer/allocate 8)
                                (.putLong (long n))
                                .array))
          (bytes->bytes [^bytes b] b)
          (keyword->bytes [k] (string->bytes (name k)))
          (symbol->bytes [s] (string->bytes (name s)))
          (default->bytes [v] (string->bytes (str v)))]
    (condp instance? value
      String (string->bytes value)
      Long (number->bytes value)
      Integer (number->bytes value)
      clojure.lang.Keyword (keyword->bytes value)
      clojure.lang.Symbol (symbol->bytes value)
      (class (byte-array 0)) (bytes->bytes value)
      (default->bytes value))))

(def ^:private byte-array-memo
  "バイト配列変換のメモ化"
  (memoize to-byte-array))

;; --- p-adic距離計算 ---

(defn p-adic-distance
  "256進p-adic距離の計算（より効率的な実装）"
  [key1 key2]
  (let [^bytes bytes1 (byte-array-memo key1)
        ^bytes bytes2 (byte-array-memo key2)
        min-len (min (alength bytes1) (alength bytes2))
        max-len (max (alength bytes1) (alength bytes2))]
    (loop [i 0]
      (cond
        ;; 短い方を走査し終えた場合
        (>= i min-len)
        (if (= min-len max-len) 0.0 (/ 1.0 (Math/pow 256 i)))
        
        ;; バイトが異なる場合
        (not= (aget bytes1 i) (aget bytes2 i))
        (/ 1.0 (Math/pow 256 i))
        
        ;; 続行
        :else (recur (inc i))))))

(def distance-memo
  "距離計算のメモ化"
  (memoize p-adic-distance))

;; --- ウルトラメトリックキーレコード ---

(defrecord UltraKey [value byte-array hash-code]
  MetricSpace
  (distance [this other]
    (distance-memo (.value this) (.value other)))
  (ball [this radius]
    ;; 実装は文脈依存
    #{})
  (diameter [this]
    1.0)
  
  Object
  (hashCode [this] (.hash-code this))
  (equals [this other]
    (and (instance? UltraKey other)
         (Arrays/equals ^bytes (.byte-array this)
                       ^bytes (.byte-array ^UltraKey other)))))

(defn make-ultra-key
  "UltraKeyインスタンスの作成"
  [value]
  (let [byte-arr (byte-array-memo value)]
    (->UltraKey value byte-arr (Arrays/hashCode byte-arr))))

;; --- ウルトラメトリック木ノード ---

(defrecord UMTreeNode [key value children level metrics]
  UltraMetricTree
  (um-insert [tree new-key new-value]
    (um-tree-insert tree new-key new-value))
  (um-search [tree search-key max-dist]
    (um-tree-search tree search-key max-dist))
  (um-k-nearest [tree search-key k]
    (um-tree-k-nearest tree search-key k))
  (um-stats [tree]
    (um-tree-analyze tree)))

;; --- 関数型スタイルの木操作 ---

(defn empty-um-tree
  "空のウルトラメトリック木を作成"
  []
  (->UMTreeNode nil nil {} 0 {:size 0 :depth 0 :max-depth 0}))

(defn um-tree-insert
  "関数型スタイルによる挿入（不変）- バグ修正版"
  [tree key value]
  (let [ultra-key (make-ultra-key key)
        ^bytes key-bytes (.byte-array ultra-key)]
    (letfn [(insert-at-level [node byte-idx]
              (if (>= byte-idx (alength key-bytes))
                ;; リーフに到達
                (let [new-node (assoc node :key ultra-key :value value)
                      size-increment (if (:value node) 0 1)] ; 上書きなら0、新規なら1
                  [new-node size-increment])
                ;; 中間ノード
                (let [byte-val (aget key-bytes byte-idx)
                      child (get (:children node) byte-val)
                      empty-child (->UMTreeNode nil nil {} (inc (:level node)) {:size 0 :depth 0})
                      [new-child size-increment] (insert-at-level (or child empty-child) (inc byte-idx))]
                  (if (pos? size-increment)
                    [(-> node
                         (assoc-in [:children byte-val] new-child)
                         (update-in [:metrics :size] (fnil + 0) size-increment)
                         (update-in [:metrics :depth] (fnil max 0) (inc (:level new-child))))
                     size-increment]
                    [(assoc-in node [:children byte-val] new-child) 0]))))]
      (first (insert-at-level tree 0)))))

(defn um-tree-search
  "距離制限付き検索 - null安全版"
  [tree search-key max-distance]
  (when tree
    (let [search-ultra-key (make-ultra-key search-key)]
      (letfn [(search-node [node candidates]
                (when node
                  (let [node-candidates (if (:value node)
                                         (let [dist (distance search-ultra-key (:key node))]
                                           (if (<= dist max-distance)
                                             (conj candidates [(:value node) dist (.value (:key node))])
                                             candidates))
                                         candidates)]
                    ;; 子ノードも探索
                    (reduce (fn [acc child]
                              (if child
                                (search-node child acc)
                                acc))
                            node-candidates
                            (vals (:children node))))))]
        (search-node tree []))))))

(defn um-tree-k-nearest
  "k近傍検索（priority-mapを使用）- 改良版"
  [tree search-key k]
  (when (and tree (pos? k))
    (let [search-ultra-key (make-ultra-key search-key)
          candidates (atom (pm/priority-map))]
      
      (letfn [(collect-candidates [node]
                (when node
                  (when (:value node)
                    (let [dist (distance search-ultra-key (:key node))
                          entry [(:value node) (.value (:key node))]]
                      (swap! candidates
                             (fn [pmap]
                               (cond
                                 (< (count pmap) k)
                                 (assoc pmap entry dist)
                                 
                                 (and (seq pmap) (< dist (second (peek (rseq pmap)))))
                                 (-> pmap
                                     (dissoc (first (peek (rseq pmap))))
                                     (assoc entry dist))
                                 
                                 :else pmap)))))
                  ;; 子ノードも探索
                  (doseq [child (vals (:children node))]
                    (when child
                      (collect-candidates child)))))]
        
        (collect-candidates tree)
        (mapv (fn [[entry dist]] (conj entry dist))
              (seq @candidates))))))

;; --- 木構造解析 ---

(defn um-tree-analyze
  "木構造の統計分析（関数型スタイル）- 改良版"
  [tree]
  (when tree
    (letfn [(analyze-rec [node depth]
              (when node
                (let [base-stats {:nodes 1
                                 :leaves (if (empty? (:children node)) 1 0)
                                 :internals (if (empty? (:children node)) 0 1)
                                 :max-depth depth
                                 :values (if (:value node) 1 0)
                                 :branching-factors (if (empty? (:children node)) 
                                                      []
                                                      [(count (filter some? (vals (:children node))))])}]
                  (if (empty? (:children node))
                    base-stats
                    (let [children-stats (keep #(when % (analyze-rec % (inc depth)))
                                              (vals (:children node)))]
                      (reduce (fn [acc child-stats]
                                (-> acc
                                    (update :nodes + (:nodes child-stats))
                                    (update :leaves + (:leaves child-stats))
                                    (update :internals + (:internals child-stats))
                                    (update :max-depth max (:max-depth child-stats))
                                    (update :values + (:values child-stats))
                                    (update :branching-factors concat (:branching-factors child-stats))))
                              base-stats
                              children-stats))))))]
      
      (let [stats (analyze-rec tree 0)]
        (if stats
          (let [bf-stats (when (seq (:branching-factors stats))
                           {:avg-branching (double (/ (reduce + (:branching-factors stats))
                                                     (count (:branching-factors stats))))
                            :max-branching (apply max (:branching-factors stats))
                            :min-branching (apply min (:branching-factors stats))})]
            (merge stats bf-stats))
          {:nodes 0 :leaves 0 :internals 0 :max-depth 0 :values 0})))))

;; --- 高度な検索機能 ---

(defn range-query
  "範囲検索：指定されたキーから指定距離内のすべてのエントリを検索"
  [tree center-key radius]
  (->> (um-tree-search tree center-key radius)
       (sort-by second)  ;; 距離でソート
       (mapv (fn [[value dist original-key]]
               {:value value :distance dist :key original-key}))))

(defn cluster-analysis
  "クラスタ分析：類似度に基づくグルーピング"
  [tree similarity-threshold]
  (letfn [(collect-all-entries [node]
            (let [node-entries (if (:value node)
                                 [{:key (.value (:key node)) :value (:value node)}]
                                 [])]
              (concat node-entries
                      (mapcat collect-all-entries (vals (:children node))))))]
    
    (let [all-entries (collect-all-entries tree)
          clusters (atom [])]
      
      (doseq [entry all-entries]
        (let [nearest-candidates (um-tree-search tree (:key entry) similarity-threshold)
              cluster-members (mapv (fn [[val dist orig-key]]
                                    {:key orig-key :value val :distance dist})
                                  nearest-candidates)]
          (swap! clusters conj {:center entry :members cluster-members :size (count cluster-members)})))
      
      @clusters)))

;; --- パフォーマンステストとベンチマーク ---

(defn benchmark-operations
  "各種操作のベンチマーク"
  [data-size key-space]
  (let [test-keys (repeatedly data-size #(str "key-" (rand-int key-space)))
        test-values (map #(str "value-" %) (range data-size))
        
        ;; 挿入ベンチマーク
        start-insert (System/nanoTime)
        tree (reduce (fn [t [k v]] (um-tree-insert t k v))
                     (empty-um-tree)
                     (map vector test-keys test-values))
        insert-time (/ (- (System/nanoTime) start-insert) 1000000.0)
        
        ;; 検索ベンチマーク
        search-keys (take 100 (repeatedly #(str "key-" (rand-int key-space))))
        start-search (System/nanoTime)
        search-results (doall (map #(um-tree-search tree % 0.1) search-keys))
        search-time (/ (- (System/nanoTime) start-search) 1000000.0)
        
        ;; k近傍ベンチマーク
        start-knn (System/nanoTime)
        knn-results (doall (map #(um-tree-k-nearest tree % 5) search-keys))
        knn-time (/ (- (System/nanoTime) start-knn) 1000000.0)
        
        stats (um-tree-analyze tree)]
    
    {:performance {:insert-time insert-time
                   :search-time search-time
                   :knn-time knn-time
                   :ops-per-sec {:insert (/ data-size (/ insert-time 1000))
                                :search (/ 100 (/ search-time 1000))
                                :knn (/ 100 (/ knn-time 1000))}}
     :tree-stats stats
     :hit-rate (double (/ (count (filter seq search-results)) 
                         (count search-results)))}))

;; --- REPLデモンストレーション ---

(defn repl-demo
  "REPL駆動開発のデモンストレーション"
  []
  (println "=== REPL駆動によるウルトラメトリック木デモ ===\n")
  
  ;; 段階的にデータ構造を構築
  (let [step1 (-> (empty-um-tree)
                  (um-tree-insert "apple" "red-fruit"))
        _ (println "Step 1: appleを挿入")
        _ (println "Stats:" (um-tree-stats step1))
        
        step2 (-> step1
                  (um-tree-insert "application" "software")
                  (um-tree-insert "apply" "verb"))
        _ (println "\nStep 2: applicationとapplyを追加")
        _ (println "Stats:" (um-tree-stats step2))
        
        final-tree (reduce (fn [tree [k v]]
                             (um-tree-insert tree k v))
                           step2
                           [["banana" "yellow-fruit"]
                            ["band" "music-group"]  
                            ["bandana" "headwear"]
                            ["cat" "feline"]
                            ["category" "classification"]
                            ["catch" "verb-grasp"]])
        
        _ (println "\nFinal: 全データ挿入完了")
        _ (println "Final Stats:" (um-tree-stats final-tree))
        
        ;; インタラクティブクエリ
        test-queries ["app" "ban" "cat" "xyz"]
        
        _ (println "\n=== インタラクティブクエリテスト ===")
        _ (doseq [query test-queries]
            (let [results (um-tree-search final-tree query 0.3)
                  knn-results (um-tree-k-nearest final-tree query 3)]
              (println (format "\nQuery: '%s'" query))
              (println (format "  距離0.3以内: %d件" (count results)))
              (when (seq results)
                (doseq [[value dist orig-key] (take 3 results)]
                  (println (format "    %s (dist: %.3f, key: %s)" value dist orig-key))))
              (println (format "  3近傍: %d件" (count knn-results)))
              (doseq [[value orig-key dist] (take 3 knn-results)]
                (println (format "    %s (dist: %.3f, key: %s)" value dist orig-key)))))
        
        ;; クラスタ分析
        clusters (cluster-analysis final-tree 0.5)
        _ (println "\n=== クラスタ分析 (閾値: 0.5) ===")
        _ (doseq [cluster (take 3 clusters)]
            (println (format "Center: %s -> %s (%d members)"
                             (get-in cluster [:center :key])
                             (get-in cluster [:center :value])
                             (:size cluster))))
        
        ;; パフォーマンステスト
        perf (benchmark-operations 1000 5000)
        _ (println "\n=== パフォーマンス結果 (n=1000) ===")
        _ (println (format "挿入: %.2f ms (%.0f ops/sec)" 
                           (get-in perf [:performance :insert-time])
                           (get-in perf [:performance :ops-per-sec :insert])))
        _ (println (format "検索: %.2f ms (%.0f ops/sec)"
                           (get-in perf [:performance :search-time])
                           (get-in perf [:performance :ops-per-sec :search])))
        _ (println (format "k近傍: %.2f ms (%.0f ops/sec)"
                           (get-in perf [:performance :knn-time])
                           (get-in perf [:performance :ops-per-sec :knn])))
        _ (println (format "検索ヒット率: %.1f%%"
                           (* 100 (:hit-rate perf))))]
    
    final-tree))

;; REPL実行
(def demo-tree (repl-demo))

;; REPLでの追加実験用関数
(comment
  ;; REPLで試せる追加実験
  
  ;; カスタムデータでの実験
  (def custom-tree 
    (reduce #(um-tree-insert %1 (first %2) (second %2))
            (empty-um-tree)
            [["tokyo" "capital"] ["kyoto" "ancient-capital"] 
             ["osaka" "commerce"] ["nara" "historic"]]))
  
  ;; 距離マトリックス作成
  (let [keys ["tokyo" "kyoto" "osaka"]]
    (for [k1 keys k2 keys]
      [k1 k2 (distance-memo k1 k2)]))
  
  ;; 動的クエリ
  (defn interactive-search [tree]
    (println "Enter search key (or 'quit'):")
    (let [input (read-line)]
      (when (not= input "quit")
        (let [results (um-tree-k-nearest tree input 5)]
          (println "Results:" results)
          (recur tree)))))
  
  ;; (interactive-search demo-tree)
  )
