open Core.Std;;

module FiniteShamirInt_encode =
struct
  open LazyStream
  type secret = int
  type poly = int list
  type key = int * int

  (* Encoding functions *)
  
  let to_secret (x: int) : secret =
    x;;

  (* Generates polynomial of the form f(x) = 3 + 2*x + x^2)
   * ---> [3;2;1]   *)
  let gen_poly (s: secret) (t: int) : poly =
    let rec helper (x: secret) (y: int) : poly =
      match y with
      | 1 -> [x]
      | _ -> 
	let r = (Random.int x) in
	r::(helper x (y - 1))
    in Random.self_init(); List.rev (helper s t)
  ;;

  (* Finds the largest coeff in our polynomial to help generate
   * a prime number for our finite aritmetic *)
  let max_poly_coeff (p: poly) : int =
    let rec helper (p: poly) (max: int) =
      match p with
      | [] -> max
      | h::t ->
	if h > max then helper t h
	else helper t max
    in
    helper p 0 ;;

  let eval_poly (x: int) (p: poly) (prime: int) : int =
    let rec helper (a: int) (b: poly) : int list =
      match b with
      | [] -> []
      | hd::tl ->
	hd::(helper a (List.map ~f:(fun y -> y * a) tl))
    in
    (List.fold_left (helper x p) ~f:(+) ~init:0) mod prime;;
  
  let gen_keys (s: secret) (t: int) (n: int) : (int * key list) =
    let rec helper (n: int) (p: poly) (prime: int) : key list =
      match n with
      | 0 -> []
      | _ ->
	(n, (eval_poly n p prime))::(helper (n-1) p prime)
    in
    let poly = gen_poly s t in
    let prime = gen_prime_gt (max_poly_coeff poly) in
    (prime, List.rev (helper n poly prime));;

  let rec print_keys (keys: key list) : unit =
    match keys with
    | [] -> ()
    | h::t ->
      (match h with
      | (x,y) -> Printf.printf "(%i, %i)\n" x y; print_keys t)
  ;;
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

  let decode_keys (keys: key list) : poly =
    let lag_polys = gen_lag_poly_list keys in
    let denom = scale_denoms lag_polys 1 in
    let scaled_lags = scale_lag_polys lag_polys denom in
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
