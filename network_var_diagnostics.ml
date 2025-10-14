(* network_var_diagnostics.ml *)
open Owl
open Owl.Dense.Matrix.S

(* ============================================
   データ収集・前処理
   ============================================ *)

(* メトリクス定義 *)
type metric_channels = {
  dns_latency: float array;      (* ch0: DNS解決時間 (ms) *)
  packet_loss: float array;      (* ch1: パケットロス率 (%) *)
  connection_fails: float array; (* ch2: 接続失敗回数 *)
  bandwidth: float array;        (* ch3: 帯域幅 (Mbps) *)
  tcp_retrans: float array;      (* ch4: TCP再送回数 *)
}

(* 多変量遅延埋め込み *)
let delay_embed_mult ~series ~m =
  let p = Array.length series in
  let T = Array.length series.(0) in
  let N = T - m in
  if N <= 0 then failwith "時系列が短すぎます";
  
  let x = Mat.zeros (p*m) N in
  let y = Mat.zeros p N in
  
  for t = 0 to N - 1 do
    (* X: 過去mステップの全チャネル *)
    for lag = 0 to m - 1 do
      for i = 0 to p - 1 do
        Mat.set x (lag*p + i) t series.(i).(t + m - 1 - lag)
      done
    done;
    (* Y: 次ステップの全チャネル *)
    for i = 0 to p - 1 do 
      Mat.set y i t series.(i).(t + m) 
    done
  done;
  x, y

(* VAR Ridge回帰 *)
let solve_var_ridge ~x ~y ~lambda =
  let xt = Mat.transpose x in
  let xx_t = Mat.(x *@ xt) in
  let pkm = Mat.rows xx_t in
  
  (* Ridge正則化: XX^T + λI *)
  for i = 0 to pkm - 1 do 
    Mat.set xx_t i i (Mat.get xx_t i i +. lambda) 
  done;
  
  let yxt = Mat.(y *@ xt) in
  Mat.(yxt *@ Linalg.D.inv xx_t)

(* Companion行列構築 *)
let companion_of_var ~B ~p ~m =
  let pm = p * m in
  let c = Mat.zeros pm pm in
  
  (* 上部ブロック: 単位行列でシフト *)
  for block = 0 to m - 2 do
    for i = 0 to p - 1 do
      Mat.set c (block*p + i) ((block+1)*p + i) 1.0
    done
  done;
  
  (* 最下段: VAR係数行列 *)
  for i = 0 to p - 1 do
    for j = 0 to p * m - 1 do
      Mat.set c ((m-1)*p + i) j (Mat.get B i j)
    done
  done;
  c

(* ============================================
   因果関係グラフ抽出
   ============================================ *)

(* 隣接行列（重み付き有向グラフ） *)
let adjacency_from_B ~B ~p ~m =
  let w = Mat.zeros p p in
  
  for lag = 0 to m - 1 do
    for i = 0 to p - 1 do
      for j = 0 to p - 1 do
        let coef = Mat.get B i (lag*p + j) in
        (* 全ラグの寄与を合算（絶対値） *)
        Mat.set w i j (Mat.get w i j +. abs_float coef)
      done
    done
  done;
  w

(* 閾値化して有向グラフ抽出 *)
let threshold_graph ~adj ~threshold =
  let p = Mat.rows adj in
  let edges = ref [] in
  
  for i = 0 to p - 1 do
    for j = 0 to p - 1 do
      let weight = Mat.get adj i j in
      if weight > threshold then
        edges := (j, i, weight) :: !edges  (* j → i の因果 *)
    done
  done;
  !edges

(* ============================================
   診断出力
   ============================================ *)

let channel_names = [|
  "DNS遅延";
  "パケットロス";
  "接続失敗";
  "帯域幅";
  "TCP再送"
|]

let diagnose_network ~adj ~eigvals ~threshold =
  Printf.printf "\n=== ネットワーク因果関係解析 ===\n\n";
  
  (* 1. 固有値チェック（安定性） *)
  Printf.printf "【システム安定性】\n";
  let max_eigval = Mat.max' (Mat.abs eigvals) in
  Printf.printf "最大固有値の絶対値: %.4f\n" max_eigval;
  
  if max_eigval >= 1.0 then
    Printf.printf "⚠️  警告: システム不安定（発散傾向）\n\n"
  else
    Printf.printf "✅ システム安定\n\n";
  
  (* 2. 因果関係グラフ *)
  Printf.printf "【因果関係（閾値: %.3f）】\n" threshold;
  let edges = threshold_graph ~adj ~threshold in
  
  if List.length edges = 0 then
    Printf.printf "  検出された強い因果関係なし\n\n"
  else begin
    List.iter (fun (src, dst, weight) ->
      Printf.printf "  %s → %s (強度: %.3f)\n"
        channel_names.(src)
        channel_names.(dst)
        weight
    ) (List.sort (fun (_, _, w1) (_, _, w2) -> 
        compare w2 w1) edges);  (* 強度順にソート *)
    Printf.printf "\n"
  end;
  
  (* 3. 各チャネルの影響度 *)
  Printf.printf "【各メトリクスの影響度】\n";
  let p = Mat.rows adj in
  for i = 0 to p - 1 do
    let outgoing = ref 0.0 in
    let incoming = ref 0.0 in
    for j = 0 to p - 1 do
      outgoing := !outgoing +. Mat.get adj i j;
      incoming := !incoming +. Mat.get adj j i;
    done;
    Printf.printf "  %s:\n" channel_names.(i);
    Printf.printf "    他への影響: %.3f\n" !outgoing;
    Printf.printf "    他からの影響: %.3f\n" !incoming;
  done;
  Printf.printf "\n";
  
  (* 4. 推奨対策 *)
  Printf.printf "【推奨される対策】\n";
  let out_influence = Array.init p (fun i ->
    let sum = ref 0.0 in
    for j = 0 to p - 1 do sum := !sum +. Mat.get adj i j done;
    (i, !sum)
  ) in
  Array.sort (fun (_, w1) (_, w2) -> compare w2 w1) out_influence;
  
  Printf.printf "優先度順:\n";
  for k = 0 to min 2 (p - 1) do
    let (i, weight) = out_influence.(k) in
    if weight > 0.1 then
      Printf.printf "  %d. %s を改善 (影響度: %.3f)\n" 
        (k+1) channel_names.(i) weight
  done

(* ============================================
   メイン実行
   ============================================ *)

let () =
  Printf.printf "ネットワーク障害診断ツール\n";
  Printf.printf "==========================\n\n";
  
  (* サンプルデータ生成（実際は計測データを使用） *)
  let p = 5 in  (* 5種類のメトリクス *)
  let T = 200 in
  
  (* シミュレート: DNS遅延→接続失敗の因果 *)
  let series = Array.init p (fun i ->
    Array.init T (fun t ->
      let base = Random.float 0.5 in
      match i with
      | 0 -> (* DNS遅延: ランダムスパイク *)
        if t mod 20 = 0 then 2.0 +. Random.float 1.0 else base
      | 2 -> (* 接続失敗: DNS遅延の影響を受ける *)
        if t > 0 && series.(0).(t-1) > 1.5 then 
          1.5 +. Random.float 0.5 
        else base
      | _ -> base
    )
  ) in
  
  Printf.printf "データ収集完了: %d チャネル x %d サンプル\n\n" p T;
  
  (* VAR モデル推定 *)
  let m = 4 in  (* 4ラグまで考慮 *)
  let x, y = delay_embed_mult ~series ~m in
  let lambda = 1e-2 in
  let B = solve_var_ridge ~x ~y ~lambda in
  
  Printf.printf "VAR(%d) モデル推定完了\n" m;
  Printf.printf "係数行列 B: %d x %d\n\n" (Mat.rows B) (Mat.cols B);
  
  (* Companion行列と固有値 *)
  let c = companion_of_var ~B ~p ~m in
  let eigvals = Linalg.eig C c |> fst in
  
  (* 因果関係グラフ *)
  let adj = adjacency_from_B ~B ~p ~m in
  
  (* 診断実行 *)
  diagnose_network ~adj ~eigvals ~threshold:0.15;
  
  (* 隣接行列の可視化 *)
  Printf.printf "【生の隣接行列】\n";
  Mat.print adj;
  
  Printf.printf "\n診断完了\n"
