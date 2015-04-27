open Core.Std
open BigNum
open FiniteShamirBigNum
include FiniteShamirBigNum_decode

let run_tests () =

  let sec = (fromInt 13) in
  (* let threshold = 3 in
   * let participants = 5 in *)
  let p0 = [fromInt 0] in
  let p1 = [fromInt 1] in
  let p2 = [sec; (fromInt 10); (fromInt 2)] in
  let prime = (fromInt 17) in


  (* let keys1 = [(1, (fromInt 1494)); (2, (fromInt 1942)); (3, (fromInt 2578));
	    (4, (fromInt 3402)); (5, (fromInt 4414)); (6, (fromInt 5614))] in

  let keys2 = [(1, (fromInt 1240)); (2, (fromInt 1248)); (3, (fromInt 1258));
	     (4, (fromInt 1270)); (5, (fromInt 1334)); (6, (fromInt 1300))] in *)

  (*assert(add_polys p0 p2 = p2) ;
  assert(add_polys p1 p2 = 
    [(fromInt 1235); (fromInt 166); (fromInt 94)]) ;
  assert(add_polys p2 p3 = 
    [(fromInt 2468); (fromInt 171); (fromInt 95)]) ;

  assert(neg_poly p0 = p0) ;
  assert(neg_poly p1 = [fromInt (-1)]) ;
  assert(neg_poly p2 = 
    [(fromInt (-1234)); (fromInt (-166)); (fromInt (-94))]) ;
  assert(mult_poly_int 5 p0 = p0) ;
  assert(mult_poly_int 5 p1 = [fromInt 5]) ;
  assert(mult_poly_int 5 p2 = 
    [(fromInt 6170); (fromInt 830); (fromInt 470)]) ;
  assert(mult_poly_int 5 p3 = 
    [(fromInt 6170); (fromInt 25); (fromInt 5)]) ;

  assert(mult_poly_bignum (fromInt 5) p0 = p0) ;
  assert(mult_poly_bignum (fromInt 5) p1 = [fromInt 5]) ;
  assert(mult_poly_bignum (fromInt 5) p2 
		= [(fromInt 6170); (fromInt 830); (fromInt 470)]) ;
  assert(mult_poly_bignum (fromInt 5) p3 
		= [(fromInt 6170); (fromInt 25); (fromInt 5)]) ;

  assert(div_poly_int 1 p0 = p0) ;
  assert(div_poly_int 1 p1 = p1) ;
  assert(div_poly_int 1 p2 = p2) ;
  assert(div_poly_int 2 p2 
		= [(fromInt 617); (fromInt 83); (fromInt 47)]) ;*)

  assert(mod_poly_elts prime p2 = p2);
  let p = [(fromInt 795); (fromInt 180); (fromInt (-15))] in
  assert(mod_poly_elts prime p = p2);

  assert(mult_mod_inverse prime 8 = 15);

  (*
  assert(mult_x_a_poly 1 p1 = [(fromInt 1); (fromInt 1)]) ;
  assert(mult_x_a_poly 2 p1 = [(fromInt 2); (fromInt 1)]) ;
  *)
  let l1_num = mult_x_a_poly (-3) (mult_x_a_poly (-5) p1) in
  assert(l1_num = [(fromInt 15); (fromInt (-8)); (fromInt 1)]) ;
  let l3_num = mult_x_a_poly (-1) (mult_x_a_poly (-5) p1) in
  assert(l3_num = [(fromInt 5); (fromInt (-6)); (fromInt 1)]) ;
  let l5_num = mult_x_a_poly (-1) (mult_x_a_poly (-3) p1) in
  assert(l5_num = [(fromInt 3); (fromInt (-4)); (fromInt 1)]) ;

  let key1 = (1, (fromInt 8)) in
  let key3 = (3, (fromInt 10)) in
  let key5 = (5, (fromInt 11)) in
  let p2keys = [key1; key3; key5] in

  let l1_den = gen_lagrange_denom 1 p2keys in
  assert(l1_den = 8) ;
  let l3_den = gen_lagrange_denom 3 p2keys in
  assert(l3_den = (-4)) ;
  let l5_den = gen_lagrange_denom 5 p2keys in
  assert(l5_den = 8) ;

  assert(gen_lagrange_num 1 p2keys = l1_num) ;
  assert(gen_lagrange_num 3 p2keys = l3_num) ;
  assert(gen_lagrange_num 5 p2keys = l5_num) ;

  let l1 = (l1_den, l1_num) in
  let l3 = (l3_den, l3_num) in
  let l5 = (l5_den, l5_num) in

  assert(gen_lagrange_poly key1 p2keys = l1) ;
  assert(gen_lagrange_poly key3 p2keys = l3) ;
  assert(gen_lagrange_poly key5 p2keys = l5) ;

  let lags = [l1; l3; l5] in

  assert(gen_lag_poly_list p2keys = lags) ;

  let denoms = [l5_den;(-l3_den);l1_den] in
  assert(remove_denoms lags = denoms) ;

  assert(common_denom denoms  = 8) ;

  let scalel1 = l1 in
  assert(scale_lag_poly l1 8 = scalel1) ;
  let scalel3 = (8, [(fromInt (-10)); (fromInt 12); (fromInt (-2))]) in
  assert(scale_lag_poly l3 8 = scalel3) ;
  let scalel5 = l5 in
  assert(scale_lag_poly l5 8 = scalel5) ;

  let scaledlags = [scalel1; scalel3; scalel5] in

  assert(scale_lag_polys lags 8 = scaledlags) ;

  let combl1 = [(fromInt (120)); (fromInt (-64)); (fromInt 8)] in
  let combl3 = [(fromInt (-100)); (fromInt 120); (fromInt (-20))] in  
  let combl5 = [(fromInt 33); (fromInt (-44)); (fromInt 11)] in

  let comblagys = [combl1; combl3; combl5] in
  assert(combine_lag_ys [(fromInt (8));(fromInt 10);(fromInt 11)] scaledlags 
	 = comblagys) ;

  let calcp1 = List.fold_right ~init:[fromInt 0] ~f:(add_polys) comblagys in
  assert(calcp1 = [(fromInt 53); (fromInt 12); (fromInt (-1))]) ;

  let calcp2 = mult_poly_int 15 calcp1 in

  assert(calcp2 = [(fromInt 795); (fromInt 180); (fromInt (-15))]);

  let mod_comb = mod_poly_elts prime calcp2 in
  assert(mod_comb = [(fromInt 13); (fromInt 10); (fromInt 2)]);

  assert(decode_keys prime p2keys = mod_comb) ;

  assert(decode_keys prime p2keys = p2);
;;

run_tests ();;
