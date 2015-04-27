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
      print_string "\nError: Please enter an integer value: ";
      try_read_int ()
;;

 (* Function to receive an integer value from the user, contains simple error
  * checking if a non-integer value is given, and also checks if the value
  * is less than the number of participants*)
let rec validate_threshold (n: int) =
  let x = try_read_int () in
  if (x > n)
    then (print_string "\nError: Please enter an integer number less than or
    \nequal to the number of participants: ";
    validate_threshold n)
  else (print_string "\nInitialization Complete....processing...: ";x)
;;

(* Function to receive the key values from the user during the decryption
 * phase. *)
let rec get_key_cl (count: int) (accum: (int * int) list) : (int * int) list =
  if count <= 0 then accum
  else (let () = print_string "\nEnter key x: " in
        let x = try_read_int () in
        let () = print_string "\nEnter key y: " in
        let y = try_read_int () in
          get_key_cl (count - 1) ((x,y)::accum))
;;

 (* Initialize by providing a secret, number of participants, and minimum
  * threshold required to reconstruct the secret.  Prints out all keys to the
  * console*)
let initialize () =
  let () =
    print_string "\nSHAMIR'S SECRET SHARING SCHEME: Initialization Process...
    \nGive me a secret integer: " in
  let secret = try_read_int () in
  let () =
    print_string "\nHow many participants? (integer number requested): " in
  let num_participants = try_read_int () in
  let () =print_string "\nWhat is the minimum threshold required to access the
    \nsecret? (integer number requested): " in
  let threshold = validate_threshold num_participants in 
  (secret, threshold, num_participants)
;;  

let main_encrypt () =
  let (secret, threshold, num_participants) = initialize () in
  let primekeys = FiniteShamirIntEncode.gen_keys 
    (FiniteShamirIntEncode.to_secret secret) threshold num_participants in
  let prime = fst(primekeys) in
  let keys = snd(primekeys) in
  Printf.printf "\nPrime: %i\n" prime;
  FiniteShamirIntEncode.print_keys keys
;;

(* These are the decryption phases initializtion function and main
 * function. *)

let decrypt_init () =
  let () = print_string "\nSHAMIR'S SECRET SHARING SCHEME:
    \nInitialization Decryption Process...
    \nEnter in the prime base value: " in
  let prime = try_read_int () in
  let () = print_string "\nGive me the threshold value: " in
  let threshold = try_read_int () in
    (prime, (get_key_cl threshold []))
;;

let main_decrypt () =
  let (prime, keys) = decrypt_init () in
  let secret = FiniteShamirIntDecode.get_secret (prime)
    (FiniteShamirIntDecode.to_key keys) in
  Printf.printf "secret: %i\n" secret
;;
