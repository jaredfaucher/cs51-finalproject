open Core.Std

module type SHAMIR =
sig
  type secret
  type poly
  type key
end

module type SHAMIR_DECODE =
sig
  include SHAMIR
  type lagrange_poly
  val int_int_to_key: (int*int) list -> key list
  val get_secret: key list -> int
end

module FiniteShamirInt_decode =
struct
  type secret = int
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

  (* Used to mod each coeff in a poly by some int
   * x *)
  let mod_poly_elts (x: int) (p: poly) : poly =
    List.map ~f:(fun a -> a mod x) p
  ;;
  
  (* These three functions are adapted from stackoverflow
   * for our needs. They will be used to find the multiplicative
   * modular inverse of our denominator after finding our
   * combining the lag_poly nums into one polynomial. *)
  let rec gcd (n: int) (m: int) : int =
    if m = 0 then n
    else if n > m then gcd (n-m) m
    else gcd n (m-n)
  ;;

  let rec extended_euclidean (a: int) (b: int) : (int*int*int) =
    if b = 0 then a, 1, 0
    else match (extended_euclidean b (a mod b)) with
      (d, x, y) -> d, y, x - a/b*y;;


  let rec mult_mod_inverse (prime: int) (d: int): int =
    let (x,y,inv) = extended_euclidean prime d in
    prime + inv
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

 (* This function multiplies all the denominators from each
   * lag poly by eachother to create a bigger denominator to
   * help us avoid division errors when calculating our secret.
   * E.G. if our 3 lag polys are (1, [1;2;3]), (2,[4;5;6]) and
   * (3,[7;8;9]), our new denominator would be 1*2*3 or 6. *)
  let rec scale_denoms (lags: lagrange_poly list) (accum: int) : int =
    match lags with
    | [] -> accum
    | (x, _)::tl ->
      scale_denoms tl (x * accum)
  ;;
  (* This scales all our lagrange polynomial based on the denominator
   * d provided.  From the example in the previous function's comments
   * for (2,[4;5;6]) our resulting lag_poly from the denominator 6 would
   * be (6, [12;15;18]), where each coeff is scaled by a factor of 3. *)
  let scale_lag_poly (lag: lagrange_poly) (d: int) : lagrange_poly =
    match lag with
    | (x, l) ->
      let scale = d / x in
      (d, mult_poly_int scale l)
  ;;
  (* This function scales all of our lagrange polynomials the correct amount
   * based on our previous function.  The lag_polys from scale_denoms comments
   * would become (6,[6;12;18]), (6,[12;15;18]) and (6,[14;16;18]). This will
   * be useful in calculating our secret to help us avoid integer division
   * errors. *)
  let scale_lag_polys (lags: lagrange_poly list) (d: int) : lagrange_poly list =
    List.map ~f:(fun x -> scale_lag_poly x d) lags
  ;;

  let rec combine_lag_ys (ys: int list) (lags: lagrange_poly list) : poly list =
    match ys, lags with
    | [],[] -> []
    | yhd::ytl, laghd::lagtl ->
      (match laghd with
      | (_, num) ->
	(mult_poly_int yhd num)::(combine_lag_ys ytl lagtl))
    | _,_ -> failwith "not the same number of keys as lags"
  ;;
  

  (* FINISH DECODE_KEYS WITH PRIME NUMBER AND MODULO FUNCTIONS *)
  let decode_keys (p: prime) (keys: key list) : poly =
    let lag_polys = gen_lag_poly_list keys in
    let denom = scale_denoms lag_polys 1 in
    let scaled_lags = scale_lag_polys lag_polys denom in
    let new_denom = mult_mod_inverse p denom in
    let lag_ys = List.map ~f:(get_key_y) keys in
    let num = List.fold_right ~init:[0] ~f:(add_polys) 
      (combine_lag_ys lag_ys scaled_lags) in
    div_poly_int denom num
  ;;
  
  let get_secret (keys: key list) : int =
    match decode_keys keys with
    | h::_ -> h
    | _ -> failwith "broken"
  ;;
end
 
module FiniteShamirIntDecode = (FiniteShamirInt_decode : SHAMIR_DECODE)

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
  let secret = FiniteShamirIntDecode.get_secret (
    FiniteShamirIntDecode.int_int_to_key keys) in
  Printf.printf "secret: %i\n" secret
;;

main ();;

