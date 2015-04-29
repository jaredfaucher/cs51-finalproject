open Core.Std


(* Adapted from lecture to generate primes for our
 * finite arithmetic implementation. All credit to 
 * professor and staff *)

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
