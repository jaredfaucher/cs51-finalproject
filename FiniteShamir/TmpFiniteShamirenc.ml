open Core.Std

module type SHAMIR =
sig
  type secret
  type poly
  type key
end

module type SHAMIR_ENCODE =
sig
  include SHAMIR
  val to_secret: int -> secret
  val gen_keys: secret -> int -> int -> key list
  val print_keys: key list -> unit
end

module FiniteShamirInt_encode =
struct
  open LazyStream
  type secret = int
  type poly = int list
  type key = int * int

  (* Encoding functions *)
  
  let to_secret (x: int) : secret =
    x;;

  (* Generates polynomial of the form f(x) = 3 + 2*x + x^2)
   * ---> [3;2;1]   *)
  let gen_poly (s: secret) (t: int) : poly =
    let rec helper (x: secret) (y: int) : poly =
      match y with
      | 1 -> [x]
      | _ -> 
	let r = (Random.int x) in
	r::(helper x (y - 1))
    in Random.self_init(); List.rev (helper s t)
  ;;

  (* Finds the largest coeff in our polynomial to help generate
   * a prime number for our finite aritmetic *)
  let max_poly_coeff (p: poly) : int =
    let rec helper (p: poly) (max: int) =
      match p with
      | [] -> max
      | h::t ->
	if h > max then helper t h
	else helper t max
    in
    helper p 0 ;;

  let eval_poly (x: int) (p: poly) (prime: int) : int =
    let rec helper (a: int) (b: poly) : int list =
      match b with
      | [] -> []
      | hd::tl ->
	hd::(helper a (List.map ~f:(fun y -> y * a) tl))
    in
    (List.fold_left (helper x p) ~f:(+) ~init:0) mod prime;;
  
  let gen_keys (s: secret) (t: int) (n: int): key list =
    let rec helper (n: int) (p: poly) (prime: int) : key list =
      match n with
      | 0 -> []
      | _ ->
	(n, (eval_poly n p prime))::(helper (n-1) p)
    in
    let poly = gen_poly s t in
    let prime = gen_prime (max_poly_coeff poly) in
    List.rev (helper n poly prime);;

  let rec print_keys (keys: key list) : unit =
    match keys with
    | [] -> ()
    | h::t ->
      (match h with
      | (x,y) -> Printf.printf "(%i, %i)\n" x y; print_keys t)
  ;;
end
 
module FiniteShamirIntEncode = (FiniteShamirInt_encode : SHAMIR_ENCODE)

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
  let keys = FiniteShamirIntEncode.gen_keys 
    (FiniteShamirIntEncode.to_secret secret) threshold num_participants in
  FiniteShamirIntEncode.print_keys keys
;;

main ();;
