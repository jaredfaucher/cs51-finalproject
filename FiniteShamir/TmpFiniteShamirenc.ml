open Core.Std

module Primes = 
struct
  type 'a str = Cons of 'a * 'a stream
  and 'a stream = ('a str) lazy_t;;

  let rec ones : int stream = lazy (Cons (1,ones));;

  let head (s:'a stream) : 'a = 
    match Lazy.force s with 
      | Cons (h,_) -> h
  ;;
  
  let tail (s:'a stream) : 'a stream = 
    match Lazy.force s with 
      | Cons (_,t) -> t
  ;;

  let rec take(n:int) (s:'a stream) : 'a = 
    if n <= 0 then head s else take (n-1) (tail s)
  ;;

  let rec first(n:int) (s:'a stream) : 'a list = 
    if n <= 0 then [] else (head s)::(first (n-1) (tail s))
  ;;

  let rec map(f:'a -> 'b) (s:'a stream) : 'b stream = 
    lazy (Cons (f (head s), map f (tail s)))
  ;;

  let rec zip (f:'a -> 'b -> 'c)  
      (s1:'a stream) (s2:'b stream) : 'c stream = 
    lazy (Cons (f (head s1) (head s2), 
                zip f (tail s1) (tail s2))) ;;

  let rec filter p s = 
    if p (head s) then 
      lazy (Cons (head s, filter p (tail s)))
    else (filter p (tail s))
  ;;
  
  let even x = (x mod 2) = 0;;

  let odd x = not(even x);;

  let rec from n = lazy (Cons (n,from (n+1))) ;;

  let nats = from 0 ;;

  let not_div_by n m = not (m mod n = 0) ;;

  let rec sieve s = 
    lazy (let h = head s in 
            Cons (h, sieve (filter (not_div_by h) (tail s))))
  ;;

  (* checks if m is gt n*)
  let gt n m =
    m > n ;;

  let primes = sieve (from 2) ;;
  
  (* filters all primes gt x out of primes *)
  let primes_gt x  = filter (gt x) primes ;;

  (* randomly picks a prime from the first 20 primes greater
   * than the l_bound value. *)
  let gen_prime_gt (l_bound: int) : int =
    let r = Random.int 20 in
    take r (primes_gt l_bound) ;;

end

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
  val gen_keys: secret -> int -> int -> int * key list
  val print_keys: key list -> unit
end

module FiniteShamirInt_encode =
struct
  open Primes
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
  
  let gen_keys (s: secret) (t: int) (n: int) : (int * key list) =
    let rec helper (n: int) (p: poly) (prime: int) : key list =
      match n with
      | 0 -> []
      | _ ->
	(n, (eval_poly n p prime))::(helper (n-1) p prime)
    in
    let poly = gen_poly s t in
    let prime = gen_prime_gt (max_poly_coeff poly) in
    (prime, List.rev (helper n poly prime));;

  let rec print_keys (keys: key list) : unit =
    match keys with
    | [] -> ()
    | h::t ->
      (match h with
      | (x,y) -> Printf.printf "(%i, %i)\n" x y; print_keys t)
  ;;
end
 
module FiniteShamirIntEncode = (FiniteShamirInt_encode : SHAMIR_ENCODE)

let rec try_read_int () =
  try read_int () with
    Failure _ -> 
      print_string "\nError: Please enter an integer value: ";
      try_read_int ()
;;

let rec validate_threshold (n: int) =
  let x = try_read_int () in
  if (x > n)
    then (print_string "\nError: Please enter an integer number less than or equal to
    \nthe number of participants: ";
    validate_threshold n)
  else (print_string "\nInitialization Complete....processing...: ";x)
;;
  
(* Initialize by providing a secret, number of participants, and minimum threshold
 * required to reconstruct the secret.  Prints out all keys to the console*)
let initialize () =
  let () = print_string "\nSHAMIR'S SECRET SHARING SCHEME: Initialization Process...
    \nGive me a secret integer: " in
  let secret = try_read_int () in
  let () = print_string "\nHow many participants (integer number requested): " in
  let num_participants = try_read_int () in
  let () = print_string "\nWhat is the minimum threshold required to access the secret
    \n(integer number requested): " in
  let threshold = validate_threshold num_participants in 
  (secret, threshold, num_participants)
;;  

let main () =
  let (secret, threshold, num_participants) = initialize () in
  let primekeys = FiniteShamirIntEncode.gen_keys 
    (FiniteShamirIntEncode.to_secret secret) threshold num_participants in
  let prime = fst(primekeys) in
  let keys = snd(primekeys) in
  Printf.printf "\nPrime: %i\n" prime;
  FiniteShamirIntEncode.print_keys keys
;;
