open Core.Std
open Modules
open BigNum

module ShamirBigNum_encode =
struct
  type secret = bignum;;
  type poly = bignum list;;
  type key = int * bignum;;

(* Encoding functions *)

  let to_secret (b: bignum) : secret =
    b
  ;;
  
  (* Generates polynomial of the form f(x) = 3 + 2*x + x^2)
   * ---> [3;2;1]   *)
  let gen_poly (s: secret) (t: int) : poly =
    let rec helper (x: secret) (y: int) : poly =
      match y with
      | 1 -> [x]
      | _ -> 
        (* Generate t-1 random numbers to be coefficients to the poly*)
	    let r = (randbignum x) in
	    r::(helper x (y - 1))
    in Random.self_init(); List.rev (helper s t)
  ;;

  (* Evaluates the outcome of a poly given an int*)
  let eval_poly (x: int) (poly: poly) : bignum =
    let rec helper (x: int) (poly: poly) : bignum list =
      match poly with
      | [] -> []
      | hd::tl ->
	    hd::(helper x (List.map ~f:(fun a -> bignumTimesInt a x) tl))
    in List.fold_left (helper x poly) ~f:(plus) ~init:(fromInt 0)
  ;;
  
  (* Generates list of n keys, one for each participant*)
  let gen_keys (s: secret) (t: int) (n: int): key list =
    let rec helper (n: int) (p: poly) : key list =
      match n with
      | 0 -> []
      | _ ->
	    (n, (eval_poly n p))::(helper (n-1) p) in
	let poly = gen_poly s t in
    List.rev (helper n poly)
  ;;

  (* Prints the list of keys to the terminal window as a side-effect*)
  let rec print_keys (keys: key list) : unit =
    match keys with
    | [] -> ()
    | h::t ->
      let (x,y) = h in
      Printf.printf "(%i, %s)\n" x (toString y); print_keys t
  ;;

end

module ShamirBigNum_decode =
struct
  type secret = bignum
  type key = int * bignum
  type poly = bignum list
  type lagrange_poly = int * poly

let rec to_key (lst: (int*bignum) list) : key list =
    match lst with
    | [] -> []
    | h::t -> h::(to_key t)
  ;;

  (* decoding functions *)
  let get_key_x (key: key) : int =
    fst(key)
  ;;

  let get_key_y (key: key) : bignum =
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
	helper xt yt ((plus xh yh)::acc)
    in List.rev (helper x y [])
  ;;

  let neg_poly (poly:poly) : poly =
    List.map ~f:(fun x -> negate  x) poly
  ;;

  let mult_poly_int (x: int) (p: poly) : poly =
    let bigx = fromInt x in
    List.map ~f:(fun a -> times_faster a bigx) p
  ;;

  let mult_poly_bignum (x:bignum) (poly:poly) : poly =
    List.map ~f:(fun a -> times_faster a x) poly
  ;;
  
  let div_poly_int (x:int) (poly:poly) : poly =
    List.map ~f:(fun a -> fst (divmod a (fromInt x))) poly
  ;;

  (* multiplies a poly by (x + a) *)
  let mult_x_a_poly (a: int) (poly: poly) : poly =
    let x_half = [fromInt 0] @ poly in
    let a_half = mult_poly_int a poly
    in add_polys x_half a_half
  ;;

  let gen_lagrange_denom (x:int) (keys: key list) : int =
    let filtered_keys = List.filter ~f:(fun k -> (get_key_x k) <> x) keys in
    let filtered_keys_xs = List.map ~f:(get_key_x) filtered_keys in
    let denom = List.map ~f:(fun a -> x - a) filtered_keys_xs in
    List.fold_left ~f:( * ) ~init: 1 denom
  ;;

  let gen_lagrange_num (x:int) (keys: key list) : poly =
    let filtered_keys = List.filter ~f:(fun k -> (get_key_x k) <> x) keys in
    let neg_filtered_keys_xs = 
      List.map ~f:(fun k -> -1*(get_key_x k)) filtered_keys in
    List.fold_right ~f:(fun a b -> mult_x_a_poly a b) ~init:([fromInt 1])
    neg_filtered_keys_xs
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

  (* returns list of the the abs value of our lag_polys denoms. *)
  
  let remove_denoms (lags: lagrange_poly list) : int list =
    let rec helper (ls: lagrange_poly list) (accum: int list) : int list =
      match ls with
      | [] -> accum
      | (x, _)::tl ->
	helper tl ((abs x)::accum)
    in
    helper lags []
  ;;

  let common_denom (denoms: int list) : int =
    let rec helper (ds: int list) (count: int) : int =
      let test_denoms = List.map ~f:(fun x -> (count mod x = 0)) ds in
      if List.for_all ~f:(fun x -> x = true) test_denoms
      then count
      else helper ds (count + 1)
    in
    helper denoms 2
      ;;

  (* This scales all our lagrange polynomial based on the denominator
   * d provided.  From the example in the previous function's comments
   * for (2,[4;5;6]) our resulting lag_poly from the denominator 6 would
   * be (6, [12;15;18]), where each coeff is scaled by a factor of 3. *)
  let scale_lag_poly (lag: lagrange_poly) (d: int) : lagrange_poly =
    match lag with
    | (x, l) ->
      let scale = d / x in
      if scale < 0
        then (d, (neg_poly (mult_poly_int (abs scale) l)))
      else (d, mult_poly_int scale l)
  ;;
   
  (* This function scales all of our lagrange polynomials the correct amount
   * based on our previous function.  The lag_polys from scale_denoms comments
   * would become (6,[6;12;18]), (6,[12;15;18]) and (6,[14;16;18]). This will
   * be useful in calculating our secret to help us avoid integer division
   * errors. *)
  let scale_lag_polys (lags: lagrange_poly list) (d: int) : lagrange_poly list =
    List.map ~f:(fun x -> scale_lag_poly x d) lags
  ;;

  let rec combine_lag_ys (ys: bignum list) (lags: lagrange_poly list) : poly list =
    match ys, lags with
    | [],[] -> []
    | yhd::ytl, laghd::lagtl ->
      (match laghd with
      | (_, num) ->
	(mult_poly_bignum yhd num)::(combine_lag_ys ytl lagtl))
    | _,_ -> failwith "not the same number of keys as lags"
  ;;

  let decode_keys (keys: key list) : poly =
    let lag_polys = gen_lag_poly_list keys in
    let denom = common_denom (remove_denoms lag_polys) in
    let scaled_lags = scale_lag_polys lag_polys denom in
    let lag_ys = List.map ~f:(get_key_y) keys in
    let num = List.fold_right ~init:[fromInt 0] ~f:(add_polys) 
      (combine_lag_ys lag_ys scaled_lags) in
    div_poly_int denom num
  ;;
  
  let get_secret (keys: key list) : bignum =
    match decode_keys keys with
    | h::_ -> h
    | _ -> failwith "broken"
  ;;
end
