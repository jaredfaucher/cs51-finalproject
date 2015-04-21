open Core.Std
open Shamirtryenc
open Shamirtrydec

print_string "\n----Shamir's Secret Sharing Scheme----\n\n
  \nWhat would you like to do?: Encryption (Press 1) or Decryption (Press 2): "

let rec start () =
  match try_read_int () with
  | 1 -> main ()
  | 2 -> main_decrypt ()
  | _ -> (print_string "Error: Incorrect entry, try again"; start ())
;;

start();;
