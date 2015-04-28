open Core.Std
open BigNum
open Modules
open ShamirBigNum
open StringConvert

module ShamirBigNumEncode = (ShamirBigNum_encode : SHAMIR_ENCODE)
module ShamirBigNumDecode = (ShamirBigNum_decode : SHAMIR_DECODE)

(* Initialize by providing a secret, number of participants, and minimum threshold
 * required to reconstruct the secret.  Prints out all keys to the console*)
let rec try_read_int () =
  try read_int () with
    Failure _ -> 
      print_string "\nError: Please enter an integer value: ";
      try_read_int ()
;;

let rec try_read_bignum () =
  try fromString(read_line ()) with
    Failure _ ->
      print_string "\nError: Please enter a bignum value: ";
      try_read_bignum ()
;;

let rec try_read_secret () =
  try read_line () with
    Failure _ -> 
      print_string "\nError: Please enter an string value: ";
      try_read_secret ()
;;

let rec validate_threshold (n: int) =
  let x = try_read_int () in
  if (x > n)
    then (print_string "\nError: Please enter an integer number less than or equal to
    \nthe number of participants: ";
    validate_threshold n)
  else (print_string "\nInitialization Complete....processing...: ";x)
;;
  
let rec get_key_cl (count: int) (accum: (int * bignum) list) : (int * bignum) list =
  if count <= 0 then accum
  else let () = print_string "\nEnter key x: " in
       let x = try_read_int () in
       let () = print_string "\nEnter key y: " in
       let y = try_read_bignum () in
       get_key_cl (count - 1) ((x,y)::accum)
	 ;; 

(* Initialize by providing a secret, number of participants, and minimum threshold
 * required to reconstruct the secret.  Prints out all keys to the console*)
let encrypt_init () =
  let () = print_string "\nSHAMIR'S SECRET SHARING SCHEME: Initialization Process...
    \nGive me a secret string: " in
  let secret = stringConvert(try_read_secret ()) in
  let () = print_string "\nHow many participants (integer number requested): " in
  let num_participants = try_read_int () in
  let () = print_string "\nWhat is the minimum threshold required to access the secret
    \n(integer number requested): " in
  let threshold = validate_threshold num_participants in 
  (secret, threshold, num_participants)
;;


let main_encrypt () =
  let (secret, threshold, num_participants) = encrypt_init () in
  let keys = ShamirBigNumEncode.gen_keys 
    (ShamirBigNumEncode.to_secret secret) threshold num_participants in
  print_string "\n";
  ShamirBigNumEncode.print_keys keys
;;


let decrypt_init () =
  let () = print_string "\nSHAMIR'S SECRET SHARING SCHEME:
    \nInitialization Decryption Process...
    \nEnter in the threshold value: " in
  let threshold = try_read_int () in
  get_key_cl threshold []
;;

let main_decrypt () =
  let keys = decrypt_init () in
  let secret = ShamirBigNumDecode.get_secret (
    ShamirBigNumDecode.to_key keys) in
  Printf.printf "secret: %s\n" (bignumConvert secret)
;;

