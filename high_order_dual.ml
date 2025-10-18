(* high_order_dual.ml
   K 階までの「高階 dual / jet（テイラー係数）」を扱う最小実装
   - eps 成分は a_n = f^{(n)}(x0) / n! を長さ K+1 のリストで保持
   - 加算は要素ごと、積は畳み込み（Cauchy convolution）で truncation
   - exp, sin, cos, log を再帰式で実装
   - 外部ライブラリ不使用（標準 List, Array, Float のみ）
   - [修正版] log_series のバグ修正とパフォーマンス改善を適用
*)

(* ユーティリティ: リスト長を K+1 に揃える *)
let pad k v lst =
  let n = List.length lst in
  if n >= k + 1 then List.sub lst 0 (k+1)
  else lst @ (List.init (k + 1 - n) (fun _ -> v))

(* 要素ごとの加算（List.map2 を使って効率化） *)
let add_series k a b =
  let a = pad k 0.0 a in
  let b = pad k 0.0 b in
  List.map2 (+.) a b

(* スカラー倍（List.map を使って効率化） *)
let scale_series k c a =
  let a = pad k 0.0 a in
  List.map (fun x -> c *. x) a

(* 畳み込み（Cauchy convolution）: (a * b)_n = sum_{i=0..n} a_i * b_{n-i}
   パフォーマンス向上のため配列を使用 *)
let mul_series k a b =
  let a_arr = Array.of_list (pad k 0.0 a) in
  let b_arr = Array.of_list (pad k 0.0 b) in
  let c = Array.make (k+1) 0.0 in
  for n = 0 to k do
    let sum = ref 0.0 in
    for i = 0 to n do
      sum := !sum +. a_arr.(i) *. b_arr.(n - i)
    done;
    c.(n) <- !sum
  done;
  Array.to_list c

(* 階乗（小さめの K を想定） *)
let factorial n =
  let rec f acc i = if i > n then acc else f (acc * i) (i + 1) in
  f 1 1

(* 級数から n 次導関数の実値を取り出す: f^{(n)} = a_n * n! *)
let nth_derivative_from_series a n =
  (List.nth a n) *. float_of_int (factorial n)

(* exp の級数：B = exp(A)
   再帰式: b_0 = exp(a_0),  b_n = (1/n) * sum_{k=1..n} k * a_k * b_{n-k} *)
let exp_series k a =
  let a_arr = Array.of_list (pad k 0.0 a) in
  let b = Array.make (k+1) 0.0 in
  b.(0) <- exp a_arr.(0);
  for n = 1 to k do
    let s = ref 0.0 in
    for k1 = 1 to n do
      s := !s +. (float_of_int k1) *. a_arr.(k1) *. b.(n - k1)
    done;
    b.(n) <- !s /. float_of_int n
  done;
  Array.to_list b

(* sin, cos の同時級数計算: S = sin(A), C = cos(A)
   再帰式:
     S_n = (1/n) * sum_{k=1..n} k * a_k * C_{n-k}
     C_n = -(1/n) * sum_{k=1..n} k * a_k * S_{n-k} *)
let sin_cos_series k a =
  let a_arr = Array.of_list (pad k 0.0 a) in
  let s = Array.make (k+1) 0.0 in
  let c = Array.make (k+1) 0.0 in
  s.(0) <- sin a_arr.(0);
  c.(0) <- cos a_arr.(0);
  for n = 1 to k do
    let ss = ref 0.0 in
    let cs = ref 0.0 in
    for k1 = 1 to n do
      let term = (float_of_int k1) *. a_arr.(k1) in
      ss := !ss +. term *. c.(n - k1);
      cs := !cs +. term *. s.(n - k1)
    done;
    s.(n) <- !ss /. float_of_int n;
    c.(n) <- -. (!cs) /. float_of_int n
  done;
  (Array.to_list s, Array.to_list c)

(* ログ級数: B = log(A) [バグ修正済み]
   関係式 A'(t) = A(t) * B'(t) から導出される再帰式を使用
   a_0 * n * b_n = n * a_n - sum_{k=1..n-1} k * b_k * a_{n-k} *)
let log_series k a =
  let a_lst = pad k 0.0 a in
  let a0 = List.nth a_lst 0 in
  if a0 <= 0.0 then invalid_arg "log_series: a0 must be > 0";
  
  let a_arr = Array.of_list a_lst in
  let b = Array.make (k+1) 0.0 in
  b.(0) <- log a0;
  
  for n = 1 to k do
    let s = ref 0.0 in
    (* sum_{k=1..n-1} k * b_k * a_{n-k} を計算 *)
    for k1 = 1 to n - 1 do
      s := !s +. (float_of_int k1) *. b.(k1) *. a_arr.(n - k1)
    done;
    
    (* b_n = (n*a_n - s) / (n*a_0) *)
    let n_float = float_of_int n in
    b.(n) <- (n_float *. a_arr.(n) -. !s) /. (n_float *. a0)
  done;
  Array.to_list b

(* ヘルパー: 定数級数（a0 = c, 他は0） *)
let const_series k c = List.init (k+1) (fun i -> if i = 0 then c else 0.0)

(* 入力値 x0 とその導関数情報（ここでは 1次のみなら eps = [x0,1,0,0..] の形）から
   任意の関数の高階導関数を得る流れの例 *)

(* 例: f(x) = sin(x) * exp(x) の k 階までの係数を計算する *)
let example k x0 =
  (* 基本の series for x: A(t) = x0 + t  -> 係数は [x0, 1, 0, 0, ...] *)
  let a = List.init (k+1) (fun i -> if i = 0 then x0 else if i = 1 then 1.0 else 0.0) in
  let sin_a, _ = sin_cos_series k a in
  let exp_a = exp_series k a in
  let s_mul_e = mul_series k sin_a exp_a in
  s_mul_e  (* 返り値は係数リスト（a_n = f^{(n)}/n!） *)

(* 小さなデモ *)
let () =
  let k = 4 in
  let x0 = 1.0 in
  let series = example k x0 in
  Printf.printf "f(x)=sin(x)*exp(x) at x=%.3f, coefficients (f^{(n)}/n!):\n" x0;
  List.iteri (fun n c ->
    let deriv = c *. float_of_int (factorial n) in
    Printf.printf " n=%d: a_n=%.8f, f^{(%d)}=%.8f\n" n c n deriv
  ) series;

  (* log_series のテスト *)
  Printf.printf "\nTesting g(x) = log(exp(x)) at x=%.3f...\n" x0;
  let x_series = List.init (k+1) (fun i -> if i = 0 then x0 else if i = 1 then 1.0 else 0.0) in
  let exp_x = exp_series k x_series in
  let log_exp_x = log_series k exp_x in
  Printf.printf "Result should be [x0, 1.0, 0.0, ...]:\n";
  List.iteri (fun n c ->
    Printf.printf " n=%d: a_n=%.8f\n" n c
  ) log_exp_x
