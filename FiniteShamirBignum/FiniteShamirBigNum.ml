open Core.Std
open Modules
open BigNum

module FiniteShamirBigNum_encode =
struct
  type secret = bignum;;
  type poly = bignum list;;
  type key = int * bignum;;

  (* Convert bignum to type secret *)
  let to_secret (b: bignum) : secret = b ;;
  
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
  
  (* Finds the largest coeff in our polynomial to help generate
   * a prime number for our finite aritmetic *)
  let max_poly_coeff (p: poly) : bignum =
    let rec helper (x: poly) (max: bignum) =
      match x with
      | [] -> max
      | h::t ->
	if greater h max then helper t h
	else helper t max
    in helper p (fromInt 0)
  ;;

  (* Evaluates a polynomial with a given key x value, and prime value *)
  let eval_poly (x: int) (poly: poly) (prime: bignum) : bignum =
    let rec helper (a: int) (b: poly) : bignum list =
      match b with
      | [] -> []
      | hd::tl ->
	    hd::(helper x (List.map ~f:(fun y -> bignumTimesInt y a) tl))
    in snd(divmod 
	     (List.fold_left (helper x poly) ~f:(plus) ~init:(fromInt 0)) 
	     prime)
  ;;
  
  (* Generate a prime and a list of keys, given a secret, a threshold, 
   * and number of participants *)
  let gen_keys (s: secret) (t: int) (n: int): bignum * (key list) =
    let rec helper (n: int) (p: poly) (prime: bignum) : key list =
      match n with
      | 0 -> []
      | _ ->
	    (n, (eval_poly n p prime))::(helper (n-1) p prime) in
	let poly = gen_poly s t in
	let max_coeff = max_poly_coeff poly in
	let prime = generateRandomPrime max_coeff 
	  (times max_coeff (fromInt 5)) in
    (prime, List.rev (helper n poly prime))
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

 (* Module contains implementation to be used in dencryption algorithm *)
module FiniteShamirBigNum_decode =
struct
  type secret = bignum
  type key = int * bignum
  type poly = bignum list
  type lagrange_poly = int * poly

  (* Create list of keys from user entered input *)
  let rec to_key (lst: (int*bignum) list) : key list =
    match lst with
    | [] -> []
    | h::t -> h::(to_key t)
  ;;

  (* Decoding functions, get first value of key pair *)
  let get_key_x (key: key) : int =
    fst(key)
  ;;

  (* Decoding functions, get second value of key pair *)
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

  (* Negates a poly *)
  let neg_poly (poly:poly) : poly =
    List.map ~f:(fun x -> negate  x) poly
  ;;

  (* Multiplies a poly with an integer *)
  let mult_poly_int (x: int) (p: poly) : poly =
    let bigx = fromInt x in
    List.map ~f:(fun a -> times_faster a bigx) p
  ;;

  (* Multiplies a poly with an bignum *)
  let mult_poly_bignum (x:bignum) (poly:poly) : poly =
    List.map ~f:(fun a -> times_faster a x) poly
  ;;

  (* Divides a poly with an integer.  Will never encounter a division by
   * zero *)
  let div_poly_int (x:int) (poly:poly) : poly =
    if x = 0 then (Printf.printf "Fatal Error: Division by zero."; exit 1)
    else List.map ~f:(fun a -> fst (divmod a (fromInt x))) poly
  ;;

  (* Used to mod each coeff in a poly by some bignum
   * x *)
  let mod_poly_elts (x: bignum) (p: poly) : poly =
    List.map ~f:(fun a ->
      if greater a (fromInt 0) then snd(divmod a x)
      else plus x (negate (snd(divmod (negate a) x)))) p
  ;; 

  (* uses the extended_eclidean algorithm to produce the multiplicative
   * modular inverse of a prime and an integer. This allows us to replace
   * out common denominator with a whole number during reconstruction.
   * eg (1/2)*f(x) mod 17 = 15*f(x) mod 17 because 15 is the multiplicative
   * modular inverse of 2^-1 mod prime. *)
  let mult_mod_inverse (prime: bignum) (d: int) : bignum =
    let (_,inv,_) = euclid prime (fromInt d) in
    plus prime inv
  ;;

  (* multiplies a poly by (x + a) *)
  let mult_x_a_poly (a: int) (poly: poly) : poly =
    let x_half = [fromInt 0] @ poly in
    let a_half = mult_poly_int a poly
    in add_polys x_half a_half
  ;;

  (* Generates a lagrange poly denomenator by multiplying all a - x 
   * in our key list together, for the key with the x-value a. 
   * eg for keys [(1, 5);(2,10);(3,15)] the lagrange denominator for 
   * (1,5) would be (1 - 2) * (1 - 3) = 2 *)
  let gen_lagrange_denom (x:int) (keys: key list) : int =
    let filtered_keys = List.filter ~f:(fun k -> (get_key_x k) <> x) keys in
    let filtered_keys_xs = List.map ~f:(get_key_x) filtered_keys in
    let denom = List.map ~f:(fun a -> x - a) filtered_keys_xs in
    List.fold_left ~f:( * ) ~init: 1 denom
  ;;

  (* Generates a lagrange poly numerator given an x key value and a list 
   * of keys, ignores denominator value by multiplying  x - a for all 
   * x-values a in our key list, besides the key with the valuex x. eg 
   * for the keys [(1, 5);(2,10);(3,15)] the lagrange numerator for (1,5) 
   * would be (x - 2)*(x - 3) =  x^2 -5x +6 *)
  let gen_lagrange_num (x:int) (keys: key list) : poly =
    let filtered_keys = List.filter ~f:(fun k -> (get_key_x k) <> x) keys in
    let neg_filtered_keys_xs = 
      List.map ~f:(fun k -> -1*(get_key_x k)) filtered_keys in
    List.fold_right ~f:(fun a b -> mult_x_a_poly a b) ~init:([fromInt 1])
    neg_filtered_keys_xs
  ;;

  (* Generates a Lagrange poly given a key and key list.  A lagrange_poly type
   * is a (Lagrange denominator, Lagrange numerator) pair *)
  let gen_lagrange_poly (key: key) (keys: key list): lagrange_poly =
    let x = get_key_x key in
    let denom = gen_lagrange_denom x keys in
    let num = gen_lagrange_num x keys in
    (denom, num)
  ;;

  (* Generates a Lagrange poly list, given a list of keys *)
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

  (* Returns the lowest common denominator, given a list of denominators*)
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
   * d provided.  From the example for (2,[4;5;6]) with a common denominator
   * of 6 would be (6, [12;15;18]), where each coeff is scaled by a factor 
   * of 3. *)
  let scale_lag_poly (lag: lagrange_poly) (d: int) : lagrange_poly =
    match lag with
    | (x, l) ->
      let scale = d / x in
      if scale < 0
        then (d, (neg_poly (mult_poly_int (abs scale) l)))
      else (d, mult_poly_int scale l)
  ;;
   
  (* This function scales all of our lagrange polynomials the correct amount
   * based on our previous function. *)
  let scale_lag_polys (lags: lagrange_poly list) (d: int) : lagrange_poly list =
    List.map ~f:(fun x -> scale_lag_poly x d) lags
  ;;
  (* Evaluates a list of polys using Lagrange method, which multiplies the
   * corresponding y value from a list of keys by their resepctive lagrange 
   * polynomial's numerator.*)
  let rec combine_lag_ys (ys: bignum list) (lags: lagrange_poly list) 
      : poly list =
    match ys, lags with
    | [],[] -> []
    | yhd::ytl, laghd::lagtl ->
      (match laghd with
      | (_, num) ->
	(mult_poly_bignum yhd num)::(combine_lag_ys ytl lagtl))
    | _,_ -> failwith "not the same number of keys as lags"
  ;;

  (* This function combines the above functions to decode a list of keys into
   * the polynomial containing the key's secret when given the prime base. *)
  let decode_keys (p: bignum) (keys: key list) : poly =
    (* generates the lagrange polynomials from a list of keys *)
    let lag_polys = gen_lag_poly_list keys in
    (* finds the common denominator from the lag_poly list *)
    let denom = common_denom (remove_denoms lag_polys) in
    (* scales the lag_polys to all have the same common denominator *)
    let scaled_lags = scale_lag_polys lag_polys denom in
    (* finds the multiplicative modular inverse of denom *)
    let inv = mult_mod_inverse p denom in
    let lag_ys = List.map ~f:(get_key_y) keys in
    (* multiplies each lag_poly numerator by it's respective key 
     * y-value and then adds them all together*)
    let num = List.fold_right ~init:[fromInt 0] ~f:(add_polys) 
      (combine_lag_ys lag_ys scaled_lags) in
    (* multies the polynomial num by inv *)
    let num_2 = mult_poly_bignum inv num in
    (* mods each coeff of num_2 by the given prime number and returns
     * the polynomial containing the secret. *)
    mod_poly_elts p num_2
  ;;

  (* Calls decode_keys and returns the secret integer from the calculated
   * polynomial *)  
  let get_secret (prime: bignum) (keys: key list) : bignum =
    match decode_keys prime keys with
    | h::_ -> h
    | _ -> failwith "broken"
  ;;
end
