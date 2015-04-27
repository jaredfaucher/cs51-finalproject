open Core.Std
open BigNum

let stringConvert (s:bytes) : bignum =
  let string = explode s in
  let length = List.length string in
  match charsToBignums string length with
  | [] -> failwith "empty string"
  | hd::_ -> hd
;;

let bignumConvert (b: bignum) : bytes =
  let chars = bignumsToChars [b] in
  implode chars
;;

