open Core.Std
open Modules
open FiniteShamirInt

 (* This module includes functions to initialize Shamir's Secret Sharing Scheme
  * encryption algorithm*)
module FiniteShamirIntEncode = (FiniteShamirInt_encode : SHAMIR_ENCODE)

 (* This module includes functions to initialize Shamir's Secret Sharing Scheme
  * dencryption algorithm*)
module FiniteShamirIntDecode = (FiniteShamirInt_decode : SHAMIR_DECODE)

 (*Function to receive an integer value from the user, contains simple error
  * checking if a non-integer value is given*)
let rec try_read_int () =
  try read_int () with
  | Failure _ -> 
      print_string "\nError: Please enter a positive integer value: ";
      try_read_int ()
;;

(* Function to validate that integer entered is positive and non-zero *)
let rec validate_int (x: int) =
  if x <= 0
  then (print_string "\nError: Please enter a non-zero positive integer value: ";
       validate_int(try_read_int ()) )
  else x
;;

(* checks if n is prime, assumes n > 0 *)
let is_prime (n: int) : bool =
  let rec check_div (x: int) (count: int) : bool =
     if count >= x then true
     else if x mod count <> 0 then check_div x (count + 1)
     else false
  in if n = 1 || n = 2 then true 
     else check_div n 2
;;

(* Function to validate that the integer entered is prime *)
let rec validate_prime (x: int) =
  if not(is_prime x)
  then (print_string "\nError: Please enter a positive integer prime: ";
	validate_prime(validate_int(try_read_int()) ))
  else x
;;

 (* Function to receive an integer value from the user, contains simple error
  * checking if a non-integer value is given, and also checks if the value
  * is less than the number of participants*)
let rec validate_threshold (n: int) =
  let x = validate_int(try_read_int ()) in
  if (x > n)
    then (print_string "\nError: Please enter a positive integer number less than or
    \nequal to the number of participants: ";
    validate_threshold n)
  else (print_string "\nInitialization Complete....processing...: ";x)
;;

(* Function to receive the key values from the user during the decryption
 * phase. *)
let rec get_key_cl (count: int) (accum: (int * int) list) : (int * int) list =
  if count <= 0 then accum
  else (let () = print_string "\nEnter key x: " in
        let x = validate_int (try_read_int ()) in
        let () = print_string "\nEnter key y: " in
        let y = validate_int (try_read_int ()) in
          get_key_cl (count - 1) ((x,y)::accum))
;;

 (* Initialize by providing a secret, number of participants, and minimum
  * threshold required to reconstruct the secret.  Prints out all keys to the
  * console*)
let encrypt_init () =
  let () =
    print_string "\nSHAMIR'S SECRET SHARING SCHEME: Initialization Process...
    \nGive me a non-zero positive secret integer: " in
  let secret = validate_int (try_read_int ()) in
  let () =
    print_string "\nHow many participants? (non-zero positive integer number requested): " in
  let num_participants = validate_int (try_read_int ()) in
  let () =print_string "\nWhat is the minimum threshold required to access the
    \nsecret? (non-zero positive integer number requested): " in
  let threshold = validate_threshold num_participants in 
  (secret, threshold, num_participants)
;;  


(* This is our main function to be called by start(). It takes in our user's
 * input and runs the encryption algorithm, printing the prime and keys to 
 * the command line. *)
let main_encrypt () =
  let (secret, threshold, num_participants) = encrypt_init () in
  let primekeys = FiniteShamirIntEncode.gen_keys 
    (FiniteShamirIntEncode.to_secret secret) threshold num_participants in
  let prime = fst(primekeys) in
  let keys = snd(primekeys) in
  Printf.printf "\nPrime: %i\n" prime;
  FiniteShamirIntEncode.print_keys keys
;;

(* These are the decryption phase's initializtion function and main
 * function. *)

let decrypt_init () =
  let () = print_string "\nSHAMIR'S SECRET SHARING SCHEME:
    \nInitialization Decryption Process...
    \nEnter in the prime base value: " in
  let prime = validate_prime(validate_int (try_read_int ())) in
  let () = print_string "\nGive me the threshold value: " in
  let threshold = validate_int (try_read_int ()) in
    (prime, (get_key_cl threshold []))
;;

let main_decrypt () =
  let (prime, keys) = decrypt_init () in
  let secret = FiniteShamirIntDecode.get_secret (prime)
    (FiniteShamirIntDecode.to_key keys) in
  Printf.printf "secret: %i\n" secret
;;
