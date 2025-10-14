(* network_metrics_collector.ml *)
open Lwt.Infix
open Dns_client_unix

(* メトリクス収集 *)
type metrics = {
  dns_latency: float;      (* DNS解決時間 *)
  packet_loss: float;      (* パケットロス率 *)
  ping_rtt: float;         (* RTT *)
  connection_fails: float; (* 接続失敗数 *)
  bandwidth: float;        (* スループット *)
}

(* 定期的にメトリクスを収集 *)
let collect_dns_latency resolver domain =
  let start = Unix.gettimeofday () in
  resolver
  |> Dns_client_unix.gethostbyname domain
  >|= fun result ->
  let elapsed = Unix.gettimeofday () -. start in
  match result with
  | Ok _ -> (elapsed, 0.0) (* latency, no failure *)
  | Error _ -> (10.0, 1.0) (* timeout as 10s, failure flag *)

let collect_metrics_series ~duration ~interval =
  let resolver = Dns_client_unix.create 
    ~nameservers:[Ipaddr.V4.of_string_exn "8.8.8.8"] () in
  
  let rec loop acc remaining =
    if remaining <= 0 then Lwt.return (List.rev acc)
    else
      collect_dns_latency resolver "www.google.com" >>= fun (lat, fail) ->
      (* 他のメトリクスも収集 *)
      let m = {
        dns_latency = lat;
        packet_loss = fail;
        ping_rtt = 0.0; (* 実装略 *)
        connection_fails = fail;
        bandwidth = 0.0; (* 実装略 *)
      } in
      Lwt_unix.sleep interval >>= fun () ->
      loop (m :: acc) (remaining - 1)
  in
  loop [] (int_of_float (duration /. interval))
