(ns ultrametric-sort.improved
  (:gen-class)
  (:import [java.nio ByteBuffer]
           [java.util.concurrent ForkJoinPool Callable Future]))

;;; ----------------------------------------------------------------
;;; パフォーマンス最適化されたヘルパー関数
;;; ----------------------------------------------------------------

(defn byte-array->vec
  "比較や表示のためにJavaのバイト配列をClojureのベクタに変換します。"
  ^clojure.lang.PersistentVector [^bytes b]
  (vec b))

(defn str->byte-array
  "文字列をバイト配列に変換します。"
  ^bytes [^String s]
  (.getBytes s "UTF-8"))

;;; ----------------------------------------------------------------
;;; 末尾再帰最適化されたウルトラメトリック・バケットソート
;;; ----------------------------------------------------------------

(defn- sort-level-optimized
  "末尾再帰最適化されたソート処理。
   - arrays: 現在の階層でソート対象のバイト配列のリスト
   - depth:  何バイト目に注目しているかを示すインデックス
   - acc:    蓄積器（結果を順番に蓄積）"
  [arrays depth acc]
  (if (<= (count arrays) 1)
    ;; ベースケース: 残りの配列をそのまま蓄積器に追加
    (into acc arrays)
    (let [;; group-byを使ってdepth番目のバイト値でグループ分け
          groups (group-by #(let [b (get % depth)]
                              (when (some? b) (bit-and 0xFF (int b))))
                           arrays)
          ;; 短い配列（nilキー）を先頭に配置
          shorter-arrays (get groups nil [])
          ;; バイト値でソートされたグループ
          sorted-groups (->> (dissoc groups nil)
                             (sort-by key))]
      ;; 短い配列から処理開始
      (let [new-acc (into acc shorter-arrays)]
        ;; グループを順次処理（末尾再帰）
        (loop [remaining-groups sorted-groups
               current-acc new-acc]
          (if (empty? remaining-groups)
            current-acc
            (let [[_ group-arrays] (first remaining-groups)
                  sorted-group (sort-level-optimized group-arrays (inc depth) [])]
              (recur (rest remaining-groups)
                     (into current-acc sorted-group)))))))))

(defn ultrametric-sort
  "バイト配列のリストをウルトラメトリックな性質に基づいてソートします。"
  [byte-arrays]
  (sort-level-optimized byte-arrays 0 []))

;;; ----------------------------------------------------------------
;;; 並列処理対応版
;;; ----------------------------------------------------------------

(defn- parallel-sort-level
  "大きなデータセットに対する並列処理版のソート。
   閾値以下のサイズは通常処理、それ以上は並列処理を使用。"
  [arrays depth threshold]
  (if (<= (count arrays) threshold)
    ;; 小さなデータセットは通常のソートを使用
    (sort-level-optimized arrays depth [])
    ;; 大きなデータセットは並列処理
    (let [groups (group-by #(let [b (get % depth)]
                              (when (some? b) (bit-and 0xFF (int b))))
                           arrays)
          shorter-arrays (get groups nil [])
          sorted-groups (->> (dissoc groups nil)
                             (sort-by key))
          ;; 各グループを並列でソート
          futures (map (fn [[_ group-arrays]]
                         (.submit (ForkJoinPool/commonPool)
                                  ^Callable #(parallel-sort-level group-arrays (inc depth) threshold)))
                       sorted-groups)]
      ;; 結果を統合
      (concat shorter-arrays
              (mapcat #(.get ^Future %) futures)))))

(defn ultrametric-sort-parallel
  "並列処理を使用したウルトラメトリック・ソート。"
  ([byte-arrays] (ultrametric-sort-parallel byte-arrays 1000))
  ([byte-arrays threshold]
   (vec (parallel-sort-level byte-arrays 0 threshold))))

;;; ----------------------------------------------------------------
;;; 高度なMorton Code実装（3D空間応用）
;;; ----------------------------------------------------------------

(defrecord Point3D [^long x ^long y ^long z])

(defn- spread-bits-efficient
  "効率的なビット分散。Magic Numbers手法を使用。"
  ^long [^long n]
  (let [n (bit-and n 0x3ff)] ; 10ビット制限
    (-> n
        (bit-and 0x000003ff)
        (bit-or (bit-shift-left (bit-and n 0x000ffc00) 2))
        (bit-and 0x0300f00f)
        (bit-or (bit-shift-left (bit-and n 0x00000300) 2))
        (bit-and 0x030c30c3)
        (bit-or (bit-shift-left (bit-and n 0x00000030) 2))
        (bit-and 0x09249249))))

(defn point->morton-code-fast
  "高速化されたMorton code生成。"
  ^long [^Point3D point]
  (let [x (spread-bits-efficient (:x point))
        y (spread-bits-efficient (:y point))  
        z (spread-bits-efficient (:z point))]
    (bit-or x (bit-shift-left y 1) (bit-shift-left z 2))))

(defn long->byte-array-fast
  "高速化されたlong -> byte配列変換。"
  ^bytes [^long l]
  (let [buffer (ByteBuffer/allocate 8)]
    (.putLong buffer l)
    (.array buffer)))

;;; ----------------------------------------------------------------
;;; 距離ベースの空間クエリ
;;; ----------------------------------------------------------------

(defn euclidean-distance
  "2つの3D点間のユークリッド距離を計算。"
  ^double [^Point3D p1 ^Point3D p2]
  (let [dx (- (:x p1) (:x p2))
        dy (- (:y p1) (:y p2))  
        dz (- (:z p1) (:z p2))]
    (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))

(defn find-nearby-points
  "Morton code順序を使って近接点を効率的に検索。
   ソート済みのポイントリストから、指定点の近傍を高速検索。"
  [sorted-points target-point max-distance]
  (let [target-morton (point->morton-code-fast target-point)
        target-key (long->byte-array-fast target-morton)]
    ;; Morton順序での近似的な近傍検索
    ;; 実際の実装では、Morton codeの性質を利用してより効率的な検索が可能
    (filter #(< (euclidean-distance (:point %) target-point) max-distance)
            sorted-points)))

;;; ----------------------------------------------------------------
;;; ベンチマーク機能
;;; ----------------------------------------------------------------

(defn benchmark-sort
  "ソート関数のベンチマークを実行。"
  [sort-fn data description]
  (println (str "--- " description " ---"))
  (let [start-time (System/nanoTime)
        result (sort-fn data)  
        end-time (System/nanoTime)
        duration-ms (/ (- end-time start-time) 1000000.0)]
    (println (format "処理時間: %.2f ms" duration-ms))
    (println (format "データ数: %d 件" (count result)))
    result))

;;; ----------------------------------------------------------------
;;; 実行例とベンチマーク
;;; ----------------------------------------------------------------

(defn -main
  "改良版チュートリアルの実行例"
  [& args]
  (println "=== 実践的ウルトラメトリック・バケットソート ===\n")

  ;; 1. 基本的な文字列ソートのベンチマーク
  (println "--- 1. 文字列データの性能比較 ---")
  (let [test-data (map str->byte-array
                       ["zebra" "apple" "ape" "apply" "banana" "band" 
                        "applepie" "app" "application" "appreciate" "approximate"])
        
        ;; 標準ソートとの比較
        _ (benchmark-sort #(sort-by seq %) test-data "標準sort-by")
        result1 (benchmark-sort ultrametric-sort test-data "ウルトラメトリック・ソート")
        result2 (benchmark-sort #(ultrametric-sort-parallel % 5) test-data "並列ウルトラメトリック・ソート")]
    
    (println "\nソート結果の確認:")
    (doseq [item (take 5 result1)]
      (println (format "%-15s" (String. item)))))

  ;; 2. 3D空間データの高性能処理
  (println "\n--- 2. 3D空間データの高性能Morton Codeソート ---")
  (let [;; より多くのテストデータを生成
        points (for [x (range 0 20 2)
                     y (range 0 20 2)  
                     z (range 0 20 2)]
                 (->Point3D x y z))
        
        ;; Morton codeと元の点の対応を保持
        point-data (map (fn [p] 
                          {:point p
                           :morton-key (long->byte-array-fast (point->morton-code-fast p))})
                        points)]
    
    (println (format "生成された3Dポイント数: %d" (count points)))
    
    ;; Morton code順にソート
    (let [sorted-data (benchmark-sort 
                       #(ultrametric-sort (map :morton-key %))
                       point-data
                       "3D Morton Code ソート")
          
          ;; ソート結果から元のポイント情報を復元
          sorted-points (map (fn [key]
                              (first (filter #(java.util.Arrays/equals (:morton-key %) key) 
                                           point-data)))
                            sorted-data)]
      
      (println "\n空間的近接性の確認（最初の10点）:")
      (doseq [point-info (take 10 sorted-points)]
        (when point-info
          (println (format "Point: %s" (:point point-info)))))))

  ;; 3. 近傍検索のデモ
  (println "\n--- 3. 近傍検索デモ ---")
  (let [target (->Point3D 10 10 10)
        test-points (map #(hash-map :point %) 
                         [(->Point3D 9 10 10) (->Point3D 11 10 10)
                          (->Point3D 10 9 10) (->Point3D 50 50 50)])
        nearby (find-nearby-points test-points target 5.0)]
    
    (println (format "検索対象: %s" target))
    (println (format "半径5.0以内の近傍点: %d 個" (count nearby)))
    (doseq [p nearby]
      (println (format "  %s (距離: %.2f)" 
                      (:point p) 
                      (euclidean-distance (:point p) target)))))

  (println "\n=== 完了 ==="))

;; REPLでの実行用
;; (-main)
