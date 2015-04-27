open Core.Std
open Bignum

let stringconvert (s: bytes) : bignum =
  let length = String.length s in
  
  let rec helper (i: int) : int list =
    if i > 0
      then (int_of_char(String.get s (length - i))) :: (helper (i - 1))
    else []
    
  in {neg = false; coeffs = (helper length)}
;;



