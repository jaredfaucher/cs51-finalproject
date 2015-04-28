open Core.Std
open BigNum
open ShamirBigNum
include ShamirBigNum_decode

let run_tests () =
    assert (to_key
      [(1, {neg = false; coeffs = [2; 9]});
       (3, {neg = false; coeffs = [1; 6]});
       (5, {neg = false; coeffs = [9; 4]})] =
      
      [(1, {neg = false; coeffs = [2; 9]});
       (3, {neg = false; coeffs = [1; 6]});
       (5, {neg = false; coeffs = [9; 4]})]
    ) ;
       
    assert (get_key_x (1, {neg = false; coeffs = [2; 9]}) = 1) ;
    assert (get_key_x (5, {neg = false; coeffs = [9; 4]}) = 5) ;   
       
    assert (get_key_y (1, {neg = false; coeffs = [2; 9]}) =
      {neg = false; coeffs = [2; 9]}) ;
    assert (get_key_y (5, {neg = false; coeffs = [9; 4]}) =
      {neg = false; coeffs = [29; 684]}) ;

    assert (add_polys 
      [{neg = false; coeffs = [1]};
       {neg = false; coeffs = [1]};
       {neg = false; coeffs = [1]}]
       
      [{neg = false; coeffs = [2]};
       {neg = false; coeffs = [2]};
       {neg = false; coeffs = [2]}] =
       
      [{neg = false; coeffs = [3]};
       {neg = false; coeffs = [3]};
       {neg = false; coeffs = [3]}]
    ) ;;

    assert (add_polys 
      [{neg = false; coeffs = [1; 4; 5; 6]};
       {neg = false; coeffs = [1; 5; 0; 0]};
       {neg = false; coeffs = [6; 6]}]
       
      [{neg = false; coeffs = [2; 2; 5; 9; 2]};
       {neg = false; coeffs = [5; 2]};
       {neg = false; coeffs = [8; 5; 9; 2]}] =
       
      [{neg = false; coeffs = [2; 4; 0; 4; 8]};
       {neg = false; coeffs = [1; 5; 5; 2]};
       {neg = false; coeffs = [8; 6; 5; 8]}]
    ) ;

    assert (neg_poly
      [{neg = false; coeffs = [1; 4; 5; 6]};
       {neg = false; coeffs = [1; 5; 0; 0]};
       {neg = false; coeffs = [6; 6]}] =
       
     [{neg = true; coeffs = [1; 4; 5; 6]};
      {neg = true; coeffs = [1; 5; 0; 0]};
      {neg = true; coeffs = [6; 6]}]
    ) ;

    assert (mult_poly_int 5
      [{neg = false; coeffs = [1; 4; 5; 6]};
       {neg = false; coeffs = [1; 5; 0; 0]};
       {neg = false; coeffs = [6; 6]}] =
       
      [{neg = false; coeffs = [7; 2; 8; 0]};
       {neg = false; coeffs = [7; 5; 0; 0]};
       {neg = false; coeffs = [3; 3; 0]}]
    ) ;

    assert (mult_poly_bignum {neg = false; coeffs = [1; 4; 5; 6]}
      [{neg = false; coeffs = [7; 2; 8; 0]};
       {neg = false; coeffs = [7; 5; 0; 0]};
       {neg = false; coeffs = [3; 3; 0]}] =
       
      [{neg = false; coeffs = [1;0;5;9;9;6;8;0]};
       {neg = false; coeffs = [1; 0; 9; 2; 0; 0; 0; 0]};
       {neg = false; coeffs = [4; 8; 0; 4; 8; 0]}]
    ) ;

    assert (div_poly_int 2
      [{neg = false; coeffs = [7; 2; 8; 0]};
       {neg = false; coeffs = [7; 5; 0; 0]};
       {neg = false; coeffs = [3; 3; 0]}] =
       
      [{neg = false; coeffs = [3; 6; 4; 0]};
       {neg = false; coeffs = [3; 7; 5; 0]};
       {neg = false; coeffs = [1; 6; 5]}]
    ) ;

    assert (mult_x_a_poly 3 
      [{neg = false; coeffs = [7; 2; 8; 0]};
       {neg = false; coeffs = [7; 5; 0; 0]};
       {neg = false; coeffs = [3; 3; 0]}] =
       
      [{neg = false; coeffs = [2; 1; 8; 4; 0]};
       {neg = false; coeffs = [2; 9; 7; 8; 0]};
       {neg = false; coeffs = [8; 4; 9; 0]};
       {neg = false; coeffs = [3; 3; 0]}]

    ) ;

    assert (gen_lagrange_denom 2
      [(2, {neg = false; coeffs = [1; 9; 4; 2]});
       (4, {neg = false; coeffs = [3; 4; 0; 2]});
       (5, {neg = false; coeffs = [4; 4; 1; 4]})] = 6
    ) ;

    assert (gen_lagrange_num 2
      [(2, {neg = false; coeffs = [1; 9; 4; 2]});
       (4, {neg = false; coeffs = [3; 4; 0; 2]});
       (5, {neg = false; coeffs = [4; 4; 1; 4]})] =
      
      [{neg = false; coeffs = [2; 0]};
       {neg = true; coeffs = [9]}; 
       {neg = false; coeffs = [1]}] 
    ) ;

    assert (gen_lagrange_poly (2, {neg = false; coeffs = [1; 9; 4; 2]})
      [(2, {neg = false; coeffs = [1; 9; 4; 2]});
       (4, {neg = false; coeffs = [3; 4; 0; 2]});
       (5, {neg = false; coeffs = [4; 4; 1; 4]})] =
      
      (6, [{neg = false; coeffs = [2; 0]};
           {neg = true; coeffs = [9]}; 
           {neg = false; coeffs = [1]}])
    ) ;

    assert (gen_lagrange_poly (4, {neg = false; coeffs = [3; 4; 0; 2]})
      [(2, {neg = false; coeffs = [1; 9; 4; 2]});
       (4, {neg = false; coeffs = [3; 4; 0; 2]});
       (5, {neg = false; coeffs = [4; 4; 1; 4]})] =
      
      ((-2), [{neg = false; coeffs = [1; 0]};
           {neg = true; coeffs = [7]}; 
           {neg = false; coeffs = [1]}])
    ) ;

    assert (gen_lag_poly_list 
      [(2, {neg = false; coeffs = [1; 9; 4; 2]});
       (4, {neg = false; coeffs = [3; 4; 0; 2]});
       (5, {neg = false; coeffs = [4; 4; 1; 4]})] =

      [(6, [{neg = false; coeffs = [2; 0]};
           {neg = true; coeffs = [9]}; 
           {neg = false; coeffs = [1]}]);
       ((-2), [{neg = false; coeffs = [1; 0]};
           {neg = true; coeffs = [7]}; 
           {neg = false; coeffs = [1]}]);
       (3, [{neg = false; coeffs = [8]};
           {neg = true; coeffs = [6]}; 
           {neg = false; coeffs = [1]}])]
    ) ;

    assert (remove_denoms
      [(6, [{neg = false; coeffs = [2; 0]};
           {neg = true; coeffs = [9]}; 
           {neg = false; coeffs = [1]}]);
       ((-2), [{neg = false; coeffs = [1; 0]};
           {neg = true; coeffs = [7]}; 
           {neg = false; coeffs = [1]}]);
       (3, [{neg = false; coeffs = [8]};
           {neg = true; coeffs = [6]}; 
           {neg = false; coeffs = [1]}])] =
      [3; 2; 6]
    ) ;

    assert (common_denom [3; 2; 6] = 6) ;
    assert (common_denom [12; 8; 3] = 24) ;

    assert (scale_lag_poly
      (6, [{neg = false; coeffs = [2; 0]};
           {neg = true; coeffs = [9]}; 
           {neg = false; coeffs = [1]}]) 6 =
       
      (6, [{neg = false; coeffs = [2; 0]};
           {neg = true; coeffs = [9]}; 
           {neg = false; coeffs = [1]}])
    ) ;

    assert (scale_lag_poly
      ((-2), [{neg = false; coeffs = [1; 0]};
           {neg = true; coeffs = [7]}; 
           {neg = false; coeffs = [1]}]) 6 =
      
      (6, [{neg = true; coeffs = [3; 0]};
           {neg = false; coeffs = [2;1]}; 
           {neg = true; coeffs = [3]}])
    ) ;;

run_tests ();;
