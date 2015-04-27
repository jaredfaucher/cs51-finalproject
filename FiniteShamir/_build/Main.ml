open Core.Std
open Initialize


(*Getting Started....*)
print_string "\n----Shamir's Secret Sharing Scheme - Finite Version----\n\n
  \nWhat would you like to do?: Encryption (Press 1) or Decryption (Press 2): "

let rec start () =
  match try_read_int () with
  | 1 -> main_encrypt ()
  | 2 -> main_decrypt ()
  | _ -> (print_string "Error: Incorrect entry, try again: "; start ())
;;

start();;
