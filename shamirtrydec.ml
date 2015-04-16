open Core.Std

module type SHAMIR_DECRYPT =
sig
  (* type secret *)
  type key
  type poly
  type lagrange_poly
  val int_int_to_key: (int*int) list -> key list
  val get_secret: key list -> int
end

module Shamirint_decode =
struct
  (*type secret = int*)
  type key = int * int
  type poly = int list
  type lagrange_poly = int * poly
  
  let rec int_int_to_key (lst: (int*int) list) : key list =
    match lst with
    | [] -> []
    | h::t -> h::(int_int_to_key t)
  ;;

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

  (* multiplies a poly by (x + a) *)
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
    let neg_filtered_keys_xs = 
      List.map ~f:(fun k -> -1*(get_key_x k)) filtered_keys in
    List.fold_right ~f:mult_x_a_poly ~init:[1] neg_filtered_keys_xs
  ;;

  let gen_lagrange_poly (key: key) (keys: key list): lagrange_poly =
    let x = get_key_x key in
    let denom = gen_lagrange_denom x keys in
    let num = gen_lagrange_num x keys in
    (denom, num)
  ;;

  let gen_lag_poly_list (keys: key list) : lagrange_poly list =
    List.map ~f:(fun x -> gen_lagrange_poly x keys) keys
  ;;

  let rec combine_lag_ys (ys:int list) (lags: lagrange_poly list) : poly list =
    match ys, lags with
    | [],[] -> []
    | yhd::ytl, laghd::lagtl ->
      (match laghd with
      | (denom, num) ->
	(div_poly_int denom (mult_poly_int yhd num))::(combine_lag_ys ytl lagtl))
    | _,_ -> failwith "not same number of keys as lags"
  ;;

  let decode_keys (keys: key list) : poly =
    let lag_polys = gen_lag_poly_list keys in
    let lag_ys = List.map ~f:(get_key_y) keys in
    List.fold_right ~init:[0] ~f:(add_polys) (combine_lag_ys lag_ys lag_polys)
  ;;
  
  let get_secret (keys: key list) : int =
    match decode_keys keys with
    | h::_ -> h
    | _ -> failwith "broken"
  ;;
end
 
module ShamirIntDecode = (Shamirint_decode : SHAMIR_DECRYPT)

let parse_args () =
  let usage () = Printf.printf
      "usage: %s key1-x key1-y key2-x key2-y ..." Sys.argv.(0); exit 1 in
  let key_num () = Printf.printf
    "please enter x's and y's for all keys"; exit 2 in
  if Array.length Sys.argv <= 1 then usage ();
  let num_of_keys = (Array.length Sys.argv) - 1 in
  if (num_of_keys mod 2) <> 0 then key_num ();
  let arg_to_key () =
    let rec helper acc count =
      if count >= (Array.length Sys.argv) then acc
      else helper ((int_of_string(Sys.argv.(count)), 
		    int_of_string(Sys.argv.(count + 1)))::acc) (count + 2)
    in helper [] 1
  in arg_to_key ()
;;

let main () =
  let keys = parse_args () in
  let secret = ShamirIntDecode.get_secret (
    ShamirIntDecode.int_int_to_key keys) in
  Printf.printf "secret: %i\n" secret
;;

main ();;
