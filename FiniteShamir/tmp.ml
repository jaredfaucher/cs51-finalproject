  let rec int_int_to_key (lst: (int*int) list) : (int * int) list =
    match lst with
    | [] -> []
    | h::t -> h::(int_int_to_key t)
  ;;

  (* decoding functions *)
  let get_key_x (key: (int * int)) : int =
    fst(key)
  ;;

  let get_key_y (key: (int * int)) : int =
    snd(key)
  ;;

  (* adds two polys together *)
  let add_polys (x:(int list)) (y:(int list)) : (int list) =
    let rec helper (x:(int list)) (y:(int list)) (acc:(int list)) : (int list) =
      match (x, y) with
      | ([],[]) -> acc
      | ([], yh::yt) -> helper [] yt (yh::acc)
      | (xh::xt, []) -> helper xt [] (xh::acc)
      | (xh::xt, yh::yt) ->
	helper xt yt ((xh+yh)::acc)
    in List.rev (helper x y [])
  ;;

  let neg_poly (poly:(int list)) : (int list) =
    List.map ~f:(fun x -> (-1) * x) poly
  ;;

  let mult_poly_int (x:int) (poly:(int list)) : (int list) =
    List.map ~f:(fun a -> a * x) poly
  ;;

  let div_poly_int (x:int) (poly:(int list)) : (int list) =
    List.map ~f:(fun a -> a / x) poly
  ;;

  (* Used to mod each coeff in a poly by some int
   * x *)
  let mod_poly_elts (x: int) (p: (int list)) : (int list) =
    List.map ~f:(fun a -> 
      if a >= 0 then a mod x
      else x - (-a mod x)) p
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
  let mult_x_a_poly (a: int) (poly: (int list)) : (int list) =
    let x_half = [0] @ poly in
    let a_half = mult_poly_int a poly in
    add_polys x_half a_half
  ;;

  let gen_lagrange_denom (x:int) (keys: (int * int) list) : int =
    let filtered_keys = List.filter ~f:(fun k -> (get_key_x k) <> x) keys in
    let filtered_keys_xs = List.map ~f:(get_key_x) filtered_keys in
    let denom = List.map ~f:(fun a -> x - a) filtered_keys_xs in
    List.fold_left ~f:( * ) ~init:1 denom
  ;;

  let gen_lagrange_num (x:int) (keys: (int * int) list) : (int list) =
    let filtered_keys = List.filter ~f:(fun k -> (get_key_x k) <> x) keys in
    let neg_filtered_keys_xs = 
      List.map ~f:(fun k -> -1*(get_key_x k)) filtered_keys in
    List.fold_right ~f:mult_x_a_poly ~init:[1] neg_filtered_keys_xs
  ;;

  let gen_lagrange_poly (key: (int * int)) (keys: (int * int) list): (int * (int list)) =
    let x = get_key_x key in
    let denom = gen_lagrange_denom x keys in
    let num = gen_lagrange_num x keys in
    (denom, num)
  ;;

  let gen_lag_poly_list (keys: (int * int) list) : (int * (int list)) list =
    List.map ~f:(fun x -> gen_lagrange_poly x keys) keys
  ;;

 (* This function multiplies all the denominators from each
   * lag poly by eachother to create a bigger denominator to
   * help us avoid division errors when calculating our secret.
   * E.G. if our 3 lag polys are (1, [1;2;3]), (2,[4;5;6]) and
   * (3,[7;8;9]), our new denominator would be 1*2*3 or 6. *)
  let rec scale_denoms (lags: (int * (int list)) list) (accum: int) : int =
    match lags with
    | [] -> accum
    | (x, _)::tl ->
      scale_denoms tl (x * accum)
  ;;
  (* This scales all our lagrange polynomial based on the denominator
   * d provided.  From the example in the previous function's comments
   * for (2,[4;5;6]) our resulting lag_poly from the denominator 6 would
   * be (6, [12;15;18]), where each coeff is scaled by a factor of 3. *)
  let scale_lag_poly (lag: (int * (int list))) (d: int) : (int * (int list)) =
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
  let scale_lag_polys (lags: (int * (int list)) list) (d: int) : (int * (int list)) list =
    List.map ~f:(fun x -> scale_lag_poly x d) lags
  ;;

  let rec combine_lag_ys (ys: int list) (lags: (int * (int list)) list) : (int list) list =
    match ys, lags with
    | [],[] -> []
    | yhd::ytl, laghd::lagtl ->
      (match laghd with
      | (_, num) ->
	(mult_poly_int yhd num)::(combine_lag_ys ytl lagtl))
    | _,_ -> failwith "not the same number of keys as lags"
  ;;
  

  (* FINISH DECODE_KEYS WITH PRIME NUMBER AND MODULO FUNCTIONS *)
  let decode_keys (p: int) (keys: (int * int) list) : (int list) =
    let lag_polys = gen_lag_poly_list keys in
    let denom = scale_denoms lag_polys 1 in
    let scaled_lags = scale_lag_polys lag_polys denom in
    let new_denom = mult_mod_inverse p denom in
    let lag_ys = List.map ~f:(get_key_y) keys in
    let num = List.fold_right ~init:[0] ~f:(add_polys) 
      (combine_lag_ys lag_ys scaled_lags) in
    let num_2 = mult_poly_int new_denom num in
    mod_poly_elts p num_2
  ;;
  
  let get_secret (prime: int) (keys: (int * int) list) : int =
    match decode_keys prime keys with
    | h::_ -> h
    | _ -> failwith "broken"
  ;;
