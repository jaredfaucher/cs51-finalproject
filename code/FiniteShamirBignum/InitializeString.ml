open Core.Std
open BigNum
open Modules
open FiniteShamirBigNum
open StringConvert

 (* This module includes functions to initialize Shamir's Secret Sharing Scheme
  * encryption algorithm*)
module FiniteShamirBigNumEncode = (FiniteShamirBigNum_encode : SHAMIR_ENCODE)

 (* This module includes functions to initialize Shamir's Secret Sharing Scheme
  * dencryption algorithm*)
module FiniteShamirBigNumDecode = (FiniteShamirBigNum_decode : SHAMIR_DECODE)

 (*Function to receive an integer value from the user, contains simple error
  * checking if a non-integer value is given*)
let rec try_read_int () =
  try read_int () with
    Failure _ -> 
      print_string "\nError: Please enter a non-zero positive integer value: ";
      try_read_int ()
;;

 (*Function to receive an bignum value from the user, contains simple error
  * checking if a non-bignum value is given*)
let rec try_read_bignum () =
  try fromString(read_line ()) with
    Failure _ -> 
      print_string "\nError: Please enter a non-zero positive bignum value: ";
      try_read_bignum ()

 (* Function to receive a secret string from the user *)
let rec try_read_secret () =
  try read_line () with
    Failure _ -> 
      print_string "\nError: Please enter a string value: ";
      try_read_secret ()
;;

(* Function to validate that integer entered is positive and non-zero *)
let rec validate_int (x: int) =
  if x <= 0
  then (print_string "\nError: Please enter a non-zero positive integer value: ";
       validate_int(try_read_int ()) )
  else x
;;

(* Function to validate that integer entered is positive and non-zero *)
let rec validate_bignum (x: bignum) =
  if (less x (fromInt 0)) || (equal x (fromInt 0))
  then (print_string "\nError: Please enter a non-zero positive bignum value: ";
       validate_bignum(try_read_bignum ()) )
  else x
;;

(* Function to validate that the bignum entered is prime *)
let rec validate_prime (x: bignum) =
  if not(isPrime x)
  then (print_string "\nError: Please enter a positive bignum prime: ";
	validate_prime(validate_bignum(try_read_bignum()) ))
  else x
;;

 (* Function to validate that threshold < number of participants *)
let rec validate_threshold (n: int) =
  let x = validate_int(try_read_int ()) in
  if (x > n)
    then (print_string "\nError: Please enter a non-zero positive integer number less than or equal to
    \nthe number of participants: ";
    validate_threshold n)
  else (print_string "\nInitialization Complete....processing...: ";x)
;;

 (* Function to receive int * bignum key pairs from the user *)
let rec get_key_cl (count: int) (accum: (int * bignum) list) : (int * bignum) list =
  if count <= 0 then accum
  else let () = print_string "\nEnter key x: " in
       let x = validate_int (try_read_int ()) in
       let () = print_string "\nEnter key y: " in
       let y = validate_bignum (try_read_bignum ()) in
       get_key_cl (count - 1) ((x,y)::accum)
	 ;; 

(* Initialize by providing a secret, number of participants, and minimum 
 * threshold required to reconstruct the secret.  Prints out all keys to 
 * the console *)
let encrypt_init () =
  let () = print_string "\nSHAMIR'S SECRET SHARING SCHEME: Initialization Process...
    \nGive me a secret string: " in
  let secret = stringConvert(try_read_secret ()) in
  let () = print_string "\nHow many participants (positive integer number requested): " in
  let num_participants = validate_int (try_read_int ()) in
  let () = print_string "\nWhat is the minimum threshold required to access the secret
    \n(positive integer number requested): " in
  let threshold = validate_threshold num_participants in 
  (secret, threshold, num_participants)
;;

(* This is our main function to be called by start(). It takes in our user's
 * input and runs the encryption algorithm, printing the keys and prime to 
 * the command line. *)
let main_encrypt () =
  let (secret, threshold, num_participants) = encrypt_init () in
  let primekeys = FiniteShamirBigNumEncode.gen_keys
    (FiniteShamirBigNumEncode.to_secret secret) threshold num_participants in
  let prime = fst(primekeys) in
  let keys = snd(primekeys) in
  Printf.printf "\nPrime: %s\n" (toString prime);
  FiniteShamirBigNumEncode.print_keys keys
;;

(* These are the decryption phase's initializtion function and main
 * function. *)
let decrypt_init () =
  let () = print_string "\nSHAMIR'S SECRET SHARING SCHEME:
    \nInitialization Decryption Process...
    \nEnter in the prime base value: " in
  let prime = validate_prime (validate_bignum (try_read_bignum ())) in
  let () = print_string "\nGive me the threshold value: " in
  let threshold = validate_int (try_read_int ()) in
    (prime, (get_key_cl threshold []))
;;

let main_decrypt () =
  let (prime, keys) = decrypt_init () in
  let secret = FiniteShamirBigNumDecode.get_secret prime
    (FiniteShamirBigNumDecode.to_key keys) in
  Printf.printf "secret: %s\n" (bignumConvert secret)
;;

