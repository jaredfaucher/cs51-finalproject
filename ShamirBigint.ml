open Core.std;;
open Big_int;;

type poly = big_int list;;
type key = big_int * big_int;;

(* encoding functions *)

(* Generates random big_int polynomial based on the secret
 * s and the threshold value t. f(x) = 123 + 4x + 5x^2 is
 * represented as [123; 4; 5] *)
let gen_poly (s: big_int) (t: big_int) : poly =
  let rec helper (s: big_int) (t: big_int) : poly =
    match t with
    | unit_big_int -> [s]
    | _ ->
      Random.self_init();
      let r = big_int_of_int (Random.int Int.max_value) in
      r::(helper s (pred_big_int t))
  in List.rev (helper s t)
;;

(* Evaluates a polynomial at x *)
let eval_poly (x: big_int) (poly: poly) : big_int =
  let rec helper (x: big_int) (poly: poly) : big_int list =
    match poly with
    | [] -> []
    | hd::tl ->
      hd::(helper x (List.map ~f:(fun a -> mult_big_int x a) tl))
  in
  List.fold_left (helper x poly) 
    ~f:(fun x y -> add_big_int x y) ~init:zero_big_int
;;

(* Generates a list of keys based on s, t and n *)
let gen_keys (s: big_int) (t: big_int) (n: big_int) : key list =
  let rec helper (n: big_int) (poly: poly) : key list =
  match n with
  | zero_big_int -> []
  | _ ->
    (int_of_big_int n, (eval_poly n poly))::(helper (pred_big_int n) poly)
  in
  let poly = gen_poly s t in
  helper n poly
;;

(* decoding functions *)
let get_key_x (key: key) : big_int =
  fst(key)
;;

let get_key_y (key: key) : big_int =
  snd(key)
;;

let add_polys (x: poly) (y: poly) : poly =
  let rec helper (x:poly) (y: poly) (acc: poly) : poly =
    match (x, y) with
    | ([], []) -> acc
    | ([], yh::yt) -> helper [] yt (yh::acc)
    | (xh::xt, []) -> helper xt [] (hd::acc)
    | (xh::xt, yh::yt) ->
      helper xt yt ((add_big_int xh yh)::acc)
  in List.rev (helper x y [])
;;

let neg_poly (poly: poly) : poly =
  List.map ~f:(fun x -> minus_big_int x) poly
;;

let mult_poly_bigint (x:big_int) (poly: poly) : poly =
  List.map ~f:(fun a -> mult_big_int a x) poly
;;

let div_poly_bigint (x:big_int) (poly: poly) : poly =
  List.map ~f:(fun a -> div_big_int x a) poly
;;

let mult_x_a_poly (a:big_int) (poly: poly) : poly =
  let x_half = [0] @ poly in
  let a_half = mult_poly_bigint a poly in
  add_polys x_half a_half
;;

let gen_lagrange_denom (x: big_int) (keys: key list) : big_int =
  let filtered_keys = List.filter ~f:(fun k -> (get_key_x k) <> x) keys in
  let filtered_keys_xs = List.map ~f:(get_key_x) filtered_keys in
  let denom = List.map ~f:(fun a -> sub_big_int x a) filtered_keys_xs in
  List.fold_left ~f:(mult_big_int) ~init:(unit_big_int) denom
;;

let gen_lagrange_num (x: big_int) (keys: key list) : poly =
  let filtered_keys = List.filter ~f:(fun k -> (get_key_x k) <> x) keys in
  let neg_filtered_keys_xs = List.map 
    ~f:(fun k -> minus_big_int (get_key_x k)) filtered_keys in
  List.fold_right ~f:mult_x_a_poly ~init:[unit_big_int] neg_filtered_keys_xs
;;

let gen_lagrange_poly (key: key) (keys: key list) : int * poly =
  let x = get_key_x key in
  let denom = gen_lagrange_denom x keys in
  let num = gen_lagrange_num x keys in
  (denom, num)
;;

let gen_lag_poly_list (keys: key list) : (int * poly) list =
  List.map ~f:(fun x -> gen_lagrange_poly x keys) keys
;;

let rec combine_lag_ys (ys:big_int list) (lags: (int*poly) list) : poly list =
  match ys, lags with
  | [],[] -> []
  | yhd::ytl, laghd::lagtl ->
    (match laghd with
    | (denom, num) ->
      (div_poly_bigint denom (mult_poly_bigint yhd num))::(combine_lag_ys tyl lagtl))
  | _,_ -> failwith "not same number of keys as lags"
;;

let decode_keys (keys: key list) : poly =
  let lag_polys = gen_lag_poly_list keys in
  let lag_ys = List.map ~f:(get_key_y) keys in
  List.fold_right ~init:[zero_big_int] ~f:(add_polys) (combine_lag_ys lag_ys lag_polys)
;;
