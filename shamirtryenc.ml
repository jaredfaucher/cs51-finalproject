open Core.Std

module type SHAMIR_ENCRYPT =
sig
  (*type secret
  type threshold
  type num_participants*)
  type poly
  type key
  val gen_keys: int -> int -> int -> key list
  val print_keys: key list -> unit
end

module Shamirint_encode =
struct
  (*type secret = int
  type threshold = int
  type num_participants = int*)
  type poly = int list;;
  type key = int * int;;

  (* Encoding functions *)

  (* Generates polynomial of the form f(x) = 3 + 2*x + x^2)
   * ---> [3;2;1]   *)
  let gen_poly (s: int) (t: int) : poly =
    Random.self_init();
    let rec helper (s: int) (t: int) : poly =
      match t with
      | 1 -> [s]
      | _ -> 
        (* Generate t-1 random numbers to be coefficients to the poly*)
	    let r = (Random.int (s * 4)) in
	    r::(helper s (t - 1))
    in List.rev (helper s t)
  ;;

  (* Evaluates the outcome of a poly given an int*)
  let eval_poly (x: int) (poly: poly) : int =
    let rec helper (x: int) (poly: poly) : int list =
      match poly with
      | [] -> []
      | hd::tl ->
	    hd::(helper x (List.map ~f:(fun a -> a * x) tl))
    in List.fold_left (helper x poly) ~f:(+) ~init:0
  ;;
  
  (* Generates list of n keys, one for each participant*)
  let gen_keys (s: int) (t: int) (n: int): key list =
    let rec helper (n: int) (poly: poly) : key list =
      match n with
      | 0 -> []
      | _ ->
	    (n, (eval_poly n poly))::(helper (n-1) poly) in
	let poly = gen_poly s t in
    List.rev (helper n poly)
  ;;

  (* Prints the list of keys to the terminal window as a side-effect*)
  let rec print_keys (keys: key list) : unit =
    match keys with
    | [] -> ()
    | h::t ->
      let (x,y) = h in
      Printf.printf "(%i, %i)\n" x y; print_keys t
  ;;
  
end
 
module ShamirIntEncode = (Shamirint_encode : SHAMIR_ENCRYPT)

(* Initialize by providing a secret, number of participants, and minimum threshold
 * required to reconstruct the secret.  Prints out all keys to the console*)
let rec initialize () =
  let () = print_string "\nSHAMIR'S SECRET SHARING SCHEME: Initialization Process...
    \nGive me a secret integer: " in
  try read_int ()
  
  let secret = read_int () in
  let () = print_string "\nHow many participants (integer number requested): " in
  let num_participants = try_read_int () in
  let () = print_string "\nWhat is the minimum threshold required to access the secret
    \n(integer number requested): " in
  let threshold = read_int () in 
  (secret, threshold, num_participants)
;;  
  (*I want to add try/with exception handling for checkig if inputs are correct
   *No need to run with arguments, This will prompt user for arguments after running
   * ./shamirtryenc.native in terminal window*)    

    
  (*  
    "usage: %s secret threshold participants\n" Sys.argv.(0); exit 1 in
  if Array.length Sys.argv <> 4 then usage ();
  let secret = int_of_string(Sys.argv.(1)) in
  let threshold = int_of_string(Sys.argv.(2)) in
  let num_participants = int_of_string(Sys.argv.(3)) in
  (secret, threshold, num_participants)
;;
*)


let main () =
  let (secret, threshold, num_participants) = initialize () in
  let keys = ShamirIntEncode.gen_keys secret threshold num_participants in
  ShamirIntEncode.print_keys keys
;;
(* example of i/o
let rec hilo n =
     let () = print_string "type a number: " in
     let i = read_int () 
     in 
       if i = n then 
         let () = print_string "BRAVO" in
         let () = print_newline ()
         in print_newline ()
       else 
         let () = 
           if i < n then 
             let () = print_string "Higher"
             in print_newline () 
           else 
             let () = print_string "Lower"
             in print_newline ()
         in hilo n 
;;

*)
main ();;
