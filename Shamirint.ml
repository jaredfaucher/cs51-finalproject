open Core.Std;;

type poly = int list;;
type key = int * int;;

(* Encoding functions *)

(* Generates polynomial of the form f(x) = 3 + 2*x + x^2)
 * ---> [3;2;1]   *)
let gen_poly (s:int) (t:int) : poly =
  let rec helper (s:int) (t:int) : poly =
  match t with
  | 1 -> [s]
  | _ -> 
    Random.self_init();
    let r = (Random.int s) in
      r::(helper s (t - 1))
  in List.rev (helper s t)
;;

let eval_poly (x: int) (poly: poly) : int =
  let rec helper (x: int) (poly: poly) : int list =
    match poly with
    | [] -> []
    | hd::tl ->
      hd::(helper x (List.map ~f:(fun a -> x*a) tl))
  in
  List.fold_left (helper x poly) ~f:(+) ~init:0;;
  
let gen_keys (s:int) (t: int) (n: int): key list =
  let rec helper (n:int) (poly: poly) : key list =
  match n with
  | 0 -> []
  | _ ->
    (n, (eval_poly n poly))::(helper (n-1) poly)
  in
  let poly = gen_poly s t in
    List.rev (helper n poly);;

(* decoding functions *)
let get_key_x (key: key) : int =
  fst(key)
;;

let get_key_y (key: key) : int =
  snd(key)
;;

(* adds two polys together *)
let add_polys (x:poly) (y:poly) : poly =
  let rec helper (x:poly) (y:poly) (acc:poly) : poly =
    match (x, y) with
    | ([],[]) -> acc
    | ([], yh::yt) -> helper [] yt (yh::acc)
    | (xh::xt, []) -> helper xt [] (xh::acc)
    | (xh::xt, yh::yt) ->
      helper xt yt ((xh+yh)::acc)
  in List.rev (helper x y [])
;;

let neg_poly (poly:poly) : poly =
  List.map ~f:(fun x -> (-1) * x) poly
;;

let mult_poly_int (x:int) (poly:poly) : poly =
  List.map ~f:(fun a -> a * x) poly
;;

let div_poly_int (x:int) (poly:poly) : poly =
  List.map ~f:(fun a -> a / x) poly
;;

fst(* multiplies a poly by (x + a) *)
let mult_x_a_poly (a: int) (poly: poly) : poly =
  let x_half = [0] @ poly in
  let a_half = mult_poly_int a poly in
  add_polys x_half a_half
;;

let gen_lagrange_denom (x:int) (keys: key list) : int =
  let filtered_keys = List.filter ~f:(fun k -> (get_key_x k) <> x) keys in
  let filtered_keys_xs = List.map ~f:(get_key_x) filtered_keys in
  let denom = List.map ~f:(fun a -> x - a) filtered_keys_xs in
  List.fold_left ~f:( * ) ~init:1 denom
;;

let gen_lagrange_num (x:int) (keys: key list) : poly =
  let filtered_keys = List.filter ~f:(fun k -> (get_key_x k) <> x) keys in
  let neg_filtered_keys_xs = List.map ~f:(fun k -> -1*(get_key_x k)) filtered_keys in
  List.fold_right ~f:mult_x_a_poly ~init:[1] neg_filtered_keys_xs
;;

let gen_lagrange_poly (key: key) (keys: key list): int * poly =
  let x = get_key_x key in
  let denom = gen_lagrange_denom x keys in
  let num = gen_lagrange_num x keys in
  (denom, num)
;;

let gen_lag_poly_list (keys: key list) : (int * poly) list =
  List.map ~f:(fun x -> gen_lagrange_poly x keys) keys
;;

let rec combine_lag_ys (ys:int list) (lags: (int * poly) list) : poly list =
  match ys, lags with
  | [],[] -> []
  | yhd::ytl, laghd::lagtl ->
    (match laghd with
    | (denom, num) ->
       div_poly_int denom (mult_poly_int yhd num))::(combine_lag_ys ytl lagtl))
  | _,_ -> failwith "not same number of keys as lags"
;;

let decode_keys (keys: key list) : poly =
  let lag_polys = gen_lag_poly_list keys in
  let lag_ys = List.map ~f:(get_key_y) keys in
  List.fold_right ~init:[0] ~f:(add_polys) (combine_lag_ys lag_ys lag_polys)
;;
