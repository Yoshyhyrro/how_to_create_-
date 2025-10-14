(* integrated_network_diagnostics.ml *)
open Lwt.Infix
open Dns_client_unix
open Owl
open Owl.Dense.Matrix.S

(* ============================================
   リアルタイムメトリクス収集
   ============================================ *)

type metrics = {
  mutable dns_latency: float;
  mutable packet_loss: float;
  mutable connection_fails: float;
  mutable ping_rtt: float;
  mutable tcp_errors: float;
}

(* DNS遅延測定 *)
let measure_dns_latency resolver domain =
  let start = Unix.gettimeofday () in
  Lwt.catch
    (fun () ->
      Lwt_unix.with_timeout 5.0 (fun () ->
        resolver
        |> Dns_client_unix.gethostbyname domain
        >|= fun result ->
        let elapsed = (Unix.gettimeofday () -. start) *. 1000.0 in
        match result with
        | Ok _ -> (elapsed, 0.0)
        | Error _ -> (5000.0, 1.0)
      )
    )
    (fun _ -> Lwt.return (5000.0, 1.0))

(* 複数ドメインで計測 *)
let collect_metrics resolver =
  let domains = [
    "www.google.com";
    "displaycatalog.mp.microsoft.com";
    "login.live.com";
  ] in
  
  Lwt_list.map_p (fun d -> measure_dns_latency resolver d) domains
  >|= fun results ->
  
  let total_latency = List.fold_left (fun acc (lat, _) -> acc +. lat) 0.0 results in
  let total_fails = List.fold_left (fun acc (_, fail) -> acc +. fail) 0.0 results in
  
  {
    dns_latency = total_latency /. float_of_int (List.length results);
    packet_loss = total_fails /. float_of_int (List.length results);
    connection_fails = total_fails;
    ping_rtt = 0.0;  (* 簡略化 *)
    tcp_errors = 0.0; (* 簡略化 *)
  }

(* 時系列収集 *)
let collect_timeseries ~duration ~interval =
  let resolver = Dns_client_unix.create 
    ~nameservers:[Ipaddr.V4.of_string_exn "8.8.8.8"] () in
  
  Printf.printf "メトリクス収集開始 (%d秒間、%d秒間隔)\n" duration interval;
  Printf.printf "Progress: ";
  flush stdout;
  
  let rec loop acc remaining =
    if remaining <= 0 then Lwt.return (List.rev acc)
    else begin
      collect_metrics resolver >>= fun m ->
      Printf.printf ".";
      flush stdout;
      Lwt_unix.sleep (float_of_int interval) >>= fun () ->
      loop (m :: acc) (remaining - 1)
    end
  in
  
  loop [] (duration / interval)
  >|= fun data ->
  Printf.printf " 完了\n\n";
  data

(* ============================================
   VAR解析（前のartifactから流用）
   ============================================ *)

let delay_embed_mult ~series ~m =
  let p = Array.length series in
  let T = Array.length series.(0) in
  let N = T - m in
  if N <= 0 then failwith "時系列が短すぎます";
  let x = Mat.zeros (p*m) N in
  let y = Mat.zeros p N in
  for t = 0 to N - 1 do
    for lag = 0 to m - 1 do
      for i = 0 to p - 1 do
        Mat.set x (lag*p + i) t series.(i).(t + m - 1 - lag)
      done
    done;
    for i = 0 to p - 1 do Mat.set y i t series.(i).(t + m) done
  done;
  x, y

let solve_var_ridge ~x ~y ~lambda =
  let xt = Mat.transpose x in
  let xx_t = Mat.(x *@ xt) in
  let pkm = Mat.rows xx_t in
  for i = 0 to pkm - 1 do 
    Mat.set xx_t i i (Mat.get xx_t i i +. lambda) 
  done;
  let yxt = Mat.(y *@ xt) in
  Mat.(yxt *@ Linalg.D.inv xx_t)

let companion_of_var ~B ~p ~m =
  let pm = p * m in
  let c = Mat.zeros pm pm in
  for block = 0 to m - 2 do
    for i = 0 to p - 1 do
      Mat.set c (block*p + i) ((block+1)*p + i) 1.0
    done
  done;
  for i = 0 to p - 1 do
    for j = 0 to p * m - 1 do
      Mat.set c ((m-1)*p + i) j (Mat.get B i j)
    done
  done;
  c

let adjacency_from_B ~B ~p ~m =
  let w = Mat.zeros p p in
  for lag = 0 to m - 1 do
    for i = 0 to p - 1 do
      for j = 0 to p - 1 do
        let coef = Mat.get B i (lag*p + j) in
        Mat.set w i j (Mat.get w i j +. abs_float coef)
      done
    done
  done;
  w

(* ============================================
   診断ロジック
   ============================================ *)

let channel_names = [|
  "DNS遅延"; "パケットロス"; "接続失敗"; "Ping RTT"; "TCPエラー"
|]

let diagnose metrics_list =
  Printf.printf "=== VAR モデルによる因果関係解析 ===\n\n";
  
  (* メトリクスを配列に変換 *)
  let n = List.length metrics_list in
  let series = [|
    Array.of_list (List.map (fun m -> m.dns_latency) metrics_list);
    Array.of_list (List.map (fun m -> m.packet_loss) metrics_list);
    Array.of_list (List.map (fun m -> m.connection_fails) metrics_list);
    Array.of_list (List.map (fun m -> m.ping_rtt) metrics_list);
    Array.of_list (List.map (fun m -> m.tcp_errors) metrics_list);
  |] in
  
  let p = Array.length series in
  let m = min 3 (n / 10) in  (* ラグ数を動的調整 *)
  
  if n < m + 5 then begin
    Printf.printf "⚠️  データ不足（%d サンプル）\n" n;
    Printf.printf "最低 %d サンプル必要\n" (m + 5);
    exit 1
  end;
  
  Printf.printf "サンプル数: %d\n" n;
  Printf.printf "ラグ次数: %d\n\n" m;
  
  (* VAR推定 *)
  let x, y = delay_embed_mult ~series ~m in
  let lambda = 1e-2 in
  let B = solve_var_ridge ~x ~y ~lambda in
  
  (* 固有値解析 *)
  let c = companion_of_var ~B ~p ~m in
  let eigvals = Linalg.eig C c |> fst in
  let max_eig = Mat.max' (Mat.abs eigvals) in
  
  Printf.printf "【システム安定性】\n";
  Printf.printf "最大固有値: %.4f " max_eig;
  if max_eig >= 1.0 then
    Printf.printf "⚠️  不安定（問題が増幅する傾向）\n\n"
  else
    Printf.printf "✅ 安定\n\n";
  
  (* 因果グラフ *)
  let adj = adjacency_from_B ~B ~p ~m in
  
  Printf.printf "【強い因果関係（閾値 > 0.2）】\n";
  let found = ref false in
  for i = 0 to p - 1 do
    for j = 0 to p - 1 do
      let w = Mat.get adj i j in
      if w > 0.2 then begin
        Printf.printf "  %s → %s (%.3f)\n" 
          channel_names.(j) channel_names.(i) w;
        found := true
      end
    done
  done;
  if not !found then Printf.printf "  (なし)\n";
  Printf.printf "\n";
  
  (* 根本原因特定 *)
  Printf.printf "【根本原因候補】\n";
  let influences = Array.init p (fun i ->
    let out_sum = ref 0.0 in
    for j = 0 to p - 1 do
      out_sum := !out_sum +. Mat.get adj i j
    done;
    (i, !out_sum)
  ) in
  Array.sort (fun (_, w1) (_, w2) -> compare w2 w1) influences;
  
  for k = 0 to min 2 (p - 1) do
    let (i, weight) = influences.(k) in
    if weight > 0.1 then
      Printf.printf "  %d位: %s (影響度 %.3f)\n" 
        (k+1) channel_names.(i) weight
  done

(* ============================================
   メイン
   ============================================ *)

let () =
  Printf.printf "統合ネットワーク診断システム\n";
  Printf.printf "============================\n\n";
  
  Lwt_main.run begin
    collect_timeseries ~duration:120 ~interval:2
  end
  |> diagnose;
  
  Printf.printf "\n診断完了\n"
