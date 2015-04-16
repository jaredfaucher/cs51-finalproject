open Core.std

module type SHAMIR_ENCRYPT =
sig
  type secret
  type threshold
  type num_participants
  type poly
  type key
  val gen_keys: secret -> threshold -> num_participants -> key list
end

module Shamirint_encode =
struct
  type secret = int
  type threshold = int
  type num_participants = int
  type poly = int list;;
  type key = int * int;;

  (* Encoding functions *)

  (* Generates polynomial of the form f(x) = 3 + 2*x + x^2)
   * ---> [3;2;1]   *)
  let gen_poly (s: secret) (t: threshold) : poly =
    let rec helper (s: secret) (t: threshold) : poly =
      match t with
      | 1 -> [s]
      | _ -> 
	Random.self_init();
	let r = (Random.int s) in
	r::(helper s (t - 1))
    in List.rev (helper s t)
  ;;

  let eval_poly (x: int) (poly: poly) : int =
    let rec helper (x: int) (poly: poly) : int list =
      match poly with
      | [] -> []
      | hd::tl ->
	hd::(helper x (List.map ~f:(fun a -> x*a) tl))
    in
    List.fold_left (helper x poly) ~f:(+) ~init:0;;
  
  let gen_keys (s: secret) (t: threshold) (n: num_participants): key list =
    let rec helper (n: num_participants) (poly: poly) : key list =
      match n with
      | 0 -> []
      | _ ->
	(n, (eval_poly n poly))::(helper (n-1) poly)
    in
    let poly = gen_poly s t in
    List.rev (helper n poly);;
end
 
module ShamirIntEncode = (Shamirint_encode : SHAMIR_ENCRYPT)

let parse_args () =
  let usage () = Printf.printf 
    "usage: %s secret threshold participants\n" Sys.argv.(0); exit 1 in
  if Array.length Sys.argv <> 4 then usage ();
  let secret = int_of_string(Sys.argv.(1)) in
  let threshold = int_of_string(Sys.argv.(2)) in
  let num_participants = int_of_string(Sys.argv.(3)) in
  (secret, threshold, num_participants)
;;

let main () =
  let (secret, threshold, num_participants) = parse_args () in
  let keys = ShamirIntEncode.gen_keys secret threshold num_participants in
  List.map (List.rev keys) ~f:(fun x ->
    Printf.printf "(%i, %i)\n" (fst x) (snd x))
;;

main ();;
