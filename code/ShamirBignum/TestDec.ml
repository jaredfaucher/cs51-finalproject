open Core.Std
open BigNum
open ShamirBigNum
include ShamirBigNum_decode

let test_to_key () =
  let k1 = [(1, (fromInt 12345))] in
  assert(to_key k1 = [(1, (fromInt 12345))]);
  let k2 = [(2, (fromInt 6789))] in
  assert(to_key k2 = [(2, (fromInt 6789))]);
  assert(to_key (k1 @ k2) = [(1, (fromInt 12345)); (2, (fromInt 6789))])
;;

let test_get_key () =
   let k1 = (1, (fromInt 12345)) in
   assert(get_key_x k1 = 1);
   assert(get_key_y k1 = (fromInt 12345));
   let k2 = (2, (fromInt 6789)) in
   assert(get_key_x k2 = 2);
   assert(get_key_y k2 = (fromInt 6789))
;;

let test_add_polys () =
  let p0 = [(fromInt 0)] in
  let p1 = [(fromInt 1); (fromInt 1); (fromInt 1)] in
  let p2 = [(fromInt 2); (fromInt 2); (fromInt 2)] in
  assert(add_polys p0 p1 = p1);
  assert(add_polys p0 p2 = p2);
  assert(add_polys p1 p2 = [(fromInt 3); (fromInt 3); (fromInt 3)]);
  let p3 = [(fromInt 1456); (fromInt 1500); (fromInt 66)] in
  let p4 = [(fromInt 22592); (fromInt 52); (fromInt 8592)] in
  assert(add_polys p3 p4 = [(fromInt 24048); (fromInt 1552); (fromInt 8658)]);
  let p5 = [(fromInt 111); (fromInt (-222)); (fromInt 333)] in
  assert(add_polys p3 p5 = [(fromInt 1567); (fromInt 1278); (fromInt 399)]);
  let p6 = [(fromInt 4); (fromInt 5);] in
  assert(add_polys p1 p6 = [(fromInt 5); (fromInt 6); (fromInt 1)]);
  assert(add_polys p6 p1 = [(fromInt 5); (fromInt 6); (fromInt 1)])
;;

let test_neg_poly () =
  let poly1 = [(fromInt 1456); (fromInt 1500); (fromInt 66)] in
  assert(neg_poly poly1 
	 = [(fromInt (-1456)); (fromInt (-1500)); (fromInt (-66))]);
  let poly2 = [(fromInt (-1456)); (fromInt (-1500)); (fromInt (-66))] in
  assert(neg_poly poly2 = poly1)
;;

let test_mult_poly_int () =
  let poly1 = [(fromInt 1456); (fromInt 1500); (fromInt 66)] in
  assert(mult_poly_int 5 poly1 =
      [(fromInt 7280); (fromInt 7500); (fromInt 330)]);
  let poly2 = [(fromInt (-1456)); (fromInt (-1500)); (fromInt (-66))] in
  assert(mult_poly_int (-5) poly2 =
      [(fromInt 7280); (fromInt 7500); (fromInt 330)]);
  assert(mult_poly_int (-5) poly1 =
      [(fromInt (-7280)); (fromInt (-7500)); (fromInt (-330))])
;;

let test_mult_poly_bignum () =
  let big1 = (fromInt 1456) in
  let poly1 = [(fromInt 7280); (fromInt 7500); (fromInt 330)] in
  assert(mult_poly_bignum big1 poly1 =
      [(fromInt 10599680); (fromInt 10920000); (fromInt 480480)]);
  let big2 = (fromInt (-1456)) in
  let poly2 = [(fromInt (-7280)); (fromInt (-7500)); (fromInt (-330))] in
  assert(mult_poly_bignum big2 poly2 =
      [(fromInt 10599680); (fromInt 10920000); (fromInt 480480)]);
  assert(mult_poly_bignum big2 poly1 =
      [(fromInt (-10599680)); (fromInt (-10920000)); (fromInt (-480480))])
;;

let test_div_poly_int () =
  let x1 = 2 in
  let poly1 = [(fromInt 7280); (fromInt 7500); (fromInt 330)] in
  assert(div_poly_int x1 poly1 =
      [(fromInt 3640); (fromInt 3750); (fromInt 165)]);
  let x2 = (-2) in
  assert(div_poly_int x2 poly1 =
      [(fromInt (-3640)); (fromInt (-3750)); (fromInt (-165))]);
  let poly2 = [(fromInt (-7280)); (fromInt (-7500)); (fromInt (-330))] in
  assert(div_poly_int x1 poly2 =
      [(fromInt (-3640)); (fromInt (-3750)); (fromInt (-165))]);
  assert(div_poly_int x2 poly2 =
      [(fromInt 3640); (fromInt 3750); (fromInt 165)])
;;

let test_mult_x_a_poly () =
  let a1 = 3 in
  let poly1 = [(fromInt 7280); (fromInt 7500); (fromInt 330)] in
  assert(mult_x_a_poly a1 poly1 =
      [(fromInt 21840); (fromInt 29780); (fromInt 8490); (fromInt 330)]);
  let a2 = (-3) in
  assert(mult_x_a_poly a2 poly1 =
      [(fromInt (-21840)); (fromInt (-15220)); (fromInt 6510); (fromInt 330)]);
  let poly2 = [(fromInt (-7280)); (fromInt (-7500)); (fromInt (-330))] in
  assert(mult_x_a_poly a1 poly2 =
      [(fromInt (-21840)); (fromInt (-29780)); 
       (fromInt (-8490)); (fromInt (-330))]);
  assert(mult_x_a_poly a2 poly2 =
      [(fromInt 21840); (fromInt 15220);
       (fromInt (-6510)); (fromInt (-330))])
;;

let test_gen_lagrange_denom () =
  let keys = [(2, (fromInt 1942)); (4, (fromInt 3402)); (5, (fromInt 4414))] in
  assert(gen_lagrange_denom 2 keys = 6);
  assert(gen_lagrange_denom 4 keys = (-2));
  assert(gen_lagrange_denom 5 keys = 3)
;;

let test_gen_lagrange_num () =
  let keys = [(2, (fromInt 1942)); (4, (fromInt 3402)); (5, (fromInt 4414))] in
  assert(gen_lagrange_num 2 keys =
      [(fromInt 20); (fromInt (-9)); (fromInt 1)]);
  assert(gen_lagrange_num 4 keys =
      [(fromInt 10); (fromInt (-7)); (fromInt 1)]);
  assert(gen_lagrange_num 5 keys =
      [(fromInt 8); (fromInt (-6)); (fromInt 1)])
;;

let test_gen_lagrange_poly () =
  let key1 = (2, (fromInt 1942)) in
  let key2 = (4, (fromInt 3402)) in
  let key3 = (5, (fromInt 4414)) in
  let keys = [key1; key2; key3] in
  assert(gen_lagrange_poly key1 keys =
      (6, [(fromInt 20); (fromInt (-9)); (fromInt 1)]));
  assert(gen_lagrange_poly key2 keys =
      ((-2), [(fromInt 10); (fromInt (-7)); (fromInt 1)]));
  assert(gen_lagrange_poly key3 keys =
      (3, [(fromInt 8); (fromInt (-6)); (fromInt 1)]))
;;

let test_gen_lag_poly_list () =
  let key1 = (2, (fromInt 1942)) in
  let key2 = (4, (fromInt 3402)) in
  let key3 = (5, (fromInt 4414)) in
  let keys = [key1; key2; key3] in
  assert(gen_lag_poly_list keys =
      [(6, [(fromInt 20); (fromInt (-9)); (fromInt 1)]);
       ((-2), [(fromInt 10); (fromInt (-7)); (fromInt 1)]);
       (3, [(fromInt 8); (fromInt (-6)); (fromInt 1)])])
;;

let test_remove_denoms () =
  let lag1 = (6, [(fromInt 20); (fromInt (-9)); (fromInt 1)]) in
  let lag2 = ((-2), [(fromInt 10); (fromInt (-7)); (fromInt 1)]) in
  let lag3 = (3, [(fromInt 8); (fromInt (-6)); (fromInt 1)]) in
  let lags = [lag1; lag2; lag3] in
  assert(remove_denoms lags = [3; 2; 6])
;;

let test_common_denom () =
  let denoms1 = [3; 2; 6] in
  assert(common_denom denoms1 = 6);
  let denoms2 = [12; 8; 3] in
  assert(common_denom denoms2 = 24);
  let denoms3 = [5; 7; 11] in
  assert(common_denom denoms3 = 385)
;;

let test_scale_lag_poly () =
  let lag1 = (6, [(fromInt 20); (fromInt (-9)); (fromInt 1)]) in
  let lag2 = ((-2), [(fromInt 10); (fromInt (-7)); (fromInt 1)]) in
  let lag3 = (3, [(fromInt 8); (fromInt (-6)); (fromInt 1)]) in
  let lags = [lag1; lag2; lag3] in
  let common_denom = common_denom(remove_denoms lags) in
  assert(scale_lag_poly lag1 common_denom = lag1);
  assert(scale_lag_poly lag2 common_denom = 
      (6, [(fromInt (-30)); (fromInt 21); (fromInt (-3))]));
  assert(scale_lag_poly lag3 common_denom =
      (6, [(fromInt 16); (fromInt (-12)); (fromInt 2)]))
;;

let test_scale_lag_polys () =
  let lag1 = (6, [(fromInt 20); (fromInt (-9)); (fromInt 1)]) in
  let lag2 = ((-2), [(fromInt 10); (fromInt (-7)); (fromInt 1)]) in
  let lag3 = (3, [(fromInt 8); (fromInt (-6)); (fromInt 1)]) in
  let lags = [lag1; lag2; lag3] in
  let common_denom = common_denom (remove_denoms lags) in
  assert(scale_lag_polys lags common_denom =
      [lag1; (6, [(fromInt (-30)); (fromInt 21); (fromInt (-3))]);
       (6, [(fromInt 16); (fromInt (-12)); (fromInt 2)])])
;;

let test_combine_lag_ys () =
  let key1 = (2, (fromInt 1942)) in
  let key2 = (4, (fromInt 3402)) in
  let key3 = (5, (fromInt 4414)) in
  let keys = [key1; key2; key3] in
  let scaledlags = [(6, [(fromInt 20); (fromInt (-9)); (fromInt 1)]); 
		    (6, [(fromInt (-30)); (fromInt 21); (fromInt (-3))]);
		    (6, [(fromInt 16); (fromInt (-12)); (fromInt 2)])] in
  let lagys = List.map ~f:(get_key_y) keys in
  assert(combine_lag_ys lagys scaledlags =
      [[(fromInt (38840)); (fromInt (-17478)); (fromInt 1942)];
       [(fromInt (-102060)); (fromInt 71442); (fromInt (-10206))];
       [(fromInt 70624); (fromInt (-52968)); (fromInt 8828)]])
;;

let test_decode_keys () =
  let secret = (fromInt 1234) in
  let poly = [secret; (fromInt 166); (fromInt 94)] in
  let key1 = (2, (fromInt 1942)) in
  let key2 = (4, (fromInt 3402)) in
  let key3 = (5, (fromInt 4414)) in
  let keys = [key1; key2; key3] in
  assert(decode_keys keys = poly)
;;

let test_get_secret () =
  let secret = (fromInt 1234) in
  let key1 = (2, (fromInt 1942)) in
  let key2 = (4, (fromInt 3402)) in
  let key3 = (5, (fromInt 4414)) in
  let keys = [key1; key2; key3] in
  assert(get_secret keys = secret)
;;

let run_tests () =
  test_to_key ();
  test_get_key ();
  test_add_polys ();
  test_neg_poly ();
  test_mult_poly_int ();
  test_mult_poly_bignum ();
  test_div_poly_int ();
  test_mult_x_a_poly ();
  test_gen_lagrange_denom ();
  test_gen_lagrange_num ();
  test_gen_lagrange_poly ();
  test_gen_lag_poly_list ();
  test_remove_denoms ();
  test_common_denom ();
  test_scale_lag_poly ();
  test_scale_lag_polys ();
  test_combine_lag_ys ();
  test_decode_keys ();
  test_get_secret ();
;;

run_tests ()
