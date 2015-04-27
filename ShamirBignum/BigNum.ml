open Core.Std

type bignum = {neg: bool; coeffs: int list}
let base = 10

let negate (b : bignum) : bignum =
  (* if b is negative, return b with neg = false *)
  if b.neg = true then {b with neg = false}
  (* if b is 0 then just return pos representation of 0 *)
  else if b.coeffs = [] then b
  (* if b is position, return b with neg = true*)
  else {b with neg = true}
;;

let _ = assert(negate {neg = false; coeffs = []}
                    = {neg = false; coeffs = []})
let _ = assert(negate {neg = true; coeffs = [1; 2]}
                    = {neg = false; coeffs = [1; 2]})
let _ = assert(negate {neg = false; coeffs = [1; 2]}
		    = {neg = true; coeffs = [1; 2]})

let abs_bignum (b:bignum) : bignum =
  if b.neg then negate b
  else b
;;

let fromInt (n: int) : bignum =
  (* this helper function extracts digits from the int m 
     and accumulates them in lst which is returned to become
     coeffs in the returned bignum *)
  let rec helper (m: int) (lst: int list) : int list =
  match m with
  (* base case, if m is 0, return the accumulated lst *)
  | 0 -> lst
  | _ -> 
    (* this deals with multiples of base (eg 20 if base = 10) 
       it recursively calls the function using m / base while
       adding a 0 digit to the accumulated list *)
    if m mod base = 0 then helper (m / base) (0::lst)
    (* if m is not a mulptiple of base, recursively call the function
       with m / base and conn the extracted digit from m mod base to
       the accumulating list *)
    else helper (m / base) ((m mod base)::lst)
  (* if n < 0, set neg to true, else false*)
  in {neg = (n < 0); 
      (* this sets coeffs to the result of the helper function, 
         where each digit in the resulting list is made positive 
         with Int.abs and List.map *)
      coeffs = List.map ~f:Int.abs (helper n [])} 
;;

let _ = assert((fromInt 0) 
        = {neg = false; coeffs = []})
let _ = assert((fromInt 123) 
        = {neg = false; coeffs = [1; 2; 3]})
let _ = assert((fromInt (-123)) 
        = {neg = true; coeffs = [1; 2; 3]})
let _ = assert((fromInt Int.max_value) 
        = {neg = false; coeffs = [1; 0; 7; 3; 7; 4; 1; 8; 2; 3]})
let _ = assert((fromInt Int.min_value) 
        = {neg = true; coeffs = [1; 0; 7; 3; 7; 4; 1; 8; 2; 4]})

(** Some helpful functions **)

(* Removes zero coefficients from the beginning of the bignum representation *)
let rec stripzeroes (b : int list) : int list =
  match b with
    | 0 :: t -> stripzeroes t
    | _ -> b


(* stripzeroes a bignum *)
let clean (b : bignum) : bignum =
  {neg = b.neg; coeffs = stripzeroes b.coeffs}


(* Returns a random bignum from 0 to bound - 1 (inclusive).
 * Can use this to help randomly test functions. *)
let randbignum (bound : bignum) =
  let rec randbignum_rec (bound : int list) =
    match bound with
      | [] -> []
      | [h] -> if h = 0 then [] else [Random.int h]
      | _ :: t -> Random.int base :: randbignum_rec t
  in {neg = false; coeffs = List.rev (randbignum_rec (List.rev bound.coeffs))}


(** Some helpful string functions **)
(* Splits a string into a list of its characters. *)
let rec explode (s : string) : char list =
  let len = String.length s in
  if len = 0 then []
  else s.[0] :: explode (String.sub s ~pos:1 ~len:(len - 1))

(* Condenses a list of characters into a string. *)
let rec implode (cs : char list) : string =
  match cs with
    | [] -> ""
    | c :: t -> String.make 1 c ^ implode t

(** Other functions you may find useful. *)
(* Returns the first n elements of list l (or the whole list if too short) *)
let rec take_first (l : 'a list) (n : int) : 'a list =
  match l with
    | [] -> []
    | h :: t -> if n <= 0 then [] else h :: take_first t (n - 1)

(* Returns a pair
 * (first n elements of lst, rest of elements of lst) *)
let rec split lst n =
  if n = 0 then ([], lst)
  else match lst with
    | [] -> ([], [])
    | h :: t -> let (lst1, lst2) = split t (n - 1) in
                (h :: lst1, lst2)

(* Returns the floor of the base 10 log of an integer *)
let intlog (base : int) : int =
  Float.to_int (log10 (Float.of_int base))


(* fromString and toString assume the base is a power of 10 *)
(* Converts a string representing an integer to a bignum. *)
let fromString (s : string) : bignum =
  let rec fromString_rec (cs : char list) : int list =
    if cs = [] then [] else
    let (chars_to_convert, rest) = split cs (intlog base) in
    let string_to_convert = implode (List.rev chars_to_convert) in
    int_of_string string_to_convert :: fromString_rec rest
  in
  match explode s with
    | [] -> fromInt 0
    | h :: t -> if h = '-' || h = '~' then
        {neg = true; coeffs = (List.rev (fromString_rec (List.rev t)))}
      else {neg = false;
            coeffs = stripzeroes (List.rev (fromString_rec (List.rev (h :: t))))}


(* Converts a bignum to its string representation.
 * Returns a string beginning with ~ for negative integers. *)
let toString (b : bignum) : string =
  let rec pad_with_zeroes_left (s : string) (len : int) =
    if String.length s >= len then s else
      "0" ^ pad_with_zeroes_left s (len - 1) in
  let rec stripstrzeroes (s : string) (c : char) =
    if String.length s = 0 then
      "0"
    else if String.get s 0 = '0' then
      stripstrzeroes (String.sub s ~pos:1 ~len:(String.length s - 1)) c
    else s in
  let rec coeffs_to_string (coeffs : int list) : string =
    match coeffs with
      | [] -> ""
      | h :: t -> pad_with_zeroes_left (string_of_int h) (intlog base)
                  ^ coeffs_to_string t in
  let stripped = stripzeroes b.coeffs in
  if List.length stripped = 0 then "0"
  else let from_coeffs = stripstrzeroes (coeffs_to_string stripped) '0' in
       if b.neg then "~" ^ from_coeffs else from_coeffs


(*>* Problem 1.2 *>*)
let rec equal (b1 : bignum) (b2 : bignum) : bool =
  (* if b1.neg and b2.neg are not equal, automatically return false *)
  if b1.neg <> b2.neg then false
  else 
    match b1.coeffs, b2.coeffs with
    (* base case for recursive call of equal *)
    | [], [] -> true
    | b1hd::b1tl, b2hd::b2tl ->
      (* if b1hd and b2hd are the same, recursively call equal on the tails
         on the tails of b1 and b2 *)
      if b1hd = b2hd then equal {b1 with coeffs = b1tl} {b2 with coeffs = b2tl}
      else false
      (* if b1 and b2 are of different lengths, these cases will be reached *)
    | [], _ -> false
    | _, [] -> false
;;

let _ = assert((equal (fromInt 0) (fromInt 0)) = true)
let _ = assert((equal (fromInt 123) (fromInt 123)) = true)
let _ = assert((equal (fromInt (-123)) (fromInt (-123))) = true)
let _ = assert((equal (fromInt 123) (fromInt 1234)) = false)
let _ = assert((equal (fromInt 1234) (fromInt 123)) = false)
let _ = assert((equal (fromInt 123) (fromInt (-123))) = false)

let rec test_equal (count : int) (max : int) : unit =
  if count > max then ()
  else
    let _ = assert(equal (fromInt count) (fromInt max) = (count = max)) in
    test_equal (count + 1) max


let () = test_equal (-10000) 10000
let () = test_equal 10000 (-10000)
let () = test_equal (-10000) 9999

(* helper function I created to compare coeffs of two bignums. 
   Checks if |b1| < |b2| by going through each element in the 
   list seperately *)
let rec compare (b1: int list) (b2: int list) : bool =
  match b1, b2 with
  (* these two cases will not met when called through less, but are needed for
     exhaustive pattern matching *)
  | [], _ -> true
  | _, [] -> false
  | b1hd::b1tl, b2hd::b2tl ->
    (* if the two corresponding digits are the same,
       we must recursively call compare on the tails. 
       eg b1 = 1234, b2 = 1000 *)   
    if b1hd = b2hd then compare b1tl b2tl
    (* if b1hd > b2hd then b1 > b2, given they are the 
       same length, so we return false *)
    else if b1hd > b2hd then false
    (* otherwise b1 < b2 so we return true *)
    else true
;;

let less (b1 : bignum) (b2 : bignum) : bool =
  (* If b1 and b2 are equal return false *)
  if equal b1 b2 then false
  else (match b1.neg, b2.neg with
  (* if b1 is negative and b2 is positive, b1 < b2 *)
  | true, false -> true
  (* if b1 is positive and b2 is negative, b1 > b2 *)
  | false, true -> false
  (* if b1 and b2 are of the same sign, we compare their 
     lengths and use compare *)
  | false, false -> 
    (* if b1 and b2 have the same number of coeffs, we 
       use the compare function *)
    if List.length b1.coeffs = List.length b2.coeffs 
      then compare b1.coeffs b2.coeffs
    (* if b1 has less coeffs than b2 it must be smaller, 
       given no leading zeros, so we return true, otherwise 
       we return false *)
    else List.length b1.coeffs < List.length b2.coeffs
  | true, true ->
    (* if b1 and b2 have the same number of coeffs, we use 
       the compare function *)
    if List.length b2.coeffs = List.length b1.coeffs
      then compare b2.coeffs b1.coeffs
    (* if b1 has less coeffs than b2, it must be less negative, 
       and we return true, otherwise we return false *)
    else List.length b2.coeffs < List.length b1.coeffs)
;;

let _ = assert((less (fromInt 0) (fromInt 1)) = true)
let _ = assert((less (fromInt 1) (fromInt 0)) = false)
let _ = assert((less (fromInt 123) (fromInt 123)) = false)
let _ = assert((less (fromInt (-123)) (fromInt 123)) = true)
let _ = assert((less (fromInt 123) (fromInt (-123))) = false)
let _ = assert((less (fromInt 123) (fromInt 1234)) = true)
let _ = assert((less (fromInt 1234) (fromInt 123)) = false)
let _ = assert((less (fromInt (-123)) (fromInt (-1234))) = false)
let _ = assert((less (fromInt (-1234)) (fromInt (-123))) = true)

(* My greater function first checks if b1 and b2 are equal, then
   uses the less function and returns the opposite result *)
let greater (b1 : bignum) (b2 : bignum) : bool =
  if equal b1 b2 then false
  else if less b1 b2 then false
  else true
;;

let _ = assert((greater (fromInt 0) (fromInt 1)) = false)
let _ = assert((greater (fromInt 1) (fromInt 0)) = true)
let _ = assert((greater (fromInt 123) (fromInt 123)) = false)
let _ = assert((greater (fromInt (-123)) (fromInt 123)) = false)
let _ = assert((greater (fromInt 123) (fromInt (-123))) = true)
let _ = assert((greater (fromInt 123) (fromInt 1234)) = false)
let _ = assert((greater (fromInt 1234) (fromInt 123)) = true)
let _ = assert((greater (fromInt (-123)) (fromInt (-1234))) = true)
let _ = assert((greater (fromInt (-1234)) (fromInt (-123))) = false)


(*>* Problem 1.3.2 *>*)

let toInt (b : bignum) : int option =
  (* helper function expand takes int list and expands it into an int *)
  let rec expand (ints: int list) (acc: int) : int = 
  match ints with
  (* base case returns acc *)
  | [] -> acc
  (* if list is not empty, recursively call the function on tl and add
     hd to the acc * base *)
  | hd::tl -> expand tl (hd + acc*base)
  in
    (* if Int.min_value < b < Int.max_value then then call the expand 
       function *)
    if less b (fromInt Int.max_value) && greater b (fromInt Int.min_value)
    then
       (* if b is neg, return Some -1 * expand called on the coeffs *) 
       if b.neg then Some (-1*(expand b.coeffs 0))
       (* if b is pos, return Some expand called on coeffs *)
       else Some (expand b.coeffs 0)
     (* if b is Int.max_value or Int.min_value, return Some of the 
        respective value *)
     else if b = (fromInt Int.max_value) then Some Int.max_value
     else if b = (fromInt Int.min_value) then Some Int.min_value
     (* if b > Int.max_value or b < Int.min_value, return None *)
     else None
;;

let _ = assert((toInt (fromInt 0)) = Some 0)
let _ = assert((toInt (fromInt 123)) = Some 123)
let _ = assert((toInt (fromInt (-123))) = Some (-123))
let _ = assert((toInt (fromInt Int.max_value)) = Some Int.max_value)
let _ = assert((toInt (fromInt Int.min_value)) = Some Int.min_value)
let _ = assert((toInt {neg = false; 
                       coeffs = [9; 9; 9; 9; 9; 9; 9; 9; 9; 9]}) 
                = None)
let _ = assert((toInt {neg = true; 
                       coeffs = [9; 9; 9; 9; 9; 9; 9; 9; 9; 9]}) 
                = None)

(** Some arithmetic functions **)

(* Returns a bignum representing b1 + b2.
 * Assumes that b1 + b2 > 0. *)
let plus_pos (b1 : bignum) (b2 : bignum) : bignum =
  let pair_from_carry (carry : int) =
    if carry = 0 then (false, [])
    else if carry = 1 then (false, [1])
    else (true, [1])
  in
  let rec plus_with_carry (neg1, coeffs1) (neg2, coeffs2) (carry : int)
            : bool * int list =
    match (coeffs1, coeffs2) with
      | ([], []) -> pair_from_carry carry
      | ([], _) -> if carry = 0 then (neg2, coeffs2) else
          plus_with_carry (neg2, coeffs2) (pair_from_carry carry) 0
      | (_, []) -> if carry = 0 then (neg1, coeffs1) else
          plus_with_carry (neg1, coeffs1) (pair_from_carry carry) 0
      | (h1 :: t1, h2 :: t2) ->
          let (sign1, sign2) =
            ((if neg1 then -1 else 1), (if neg2 then -1 else 1)) in
          let result = h1 * sign1 + h2 * sign2 + carry in
          if result < 0 then
            let (negres, coeffsres) =
                  plus_with_carry (neg1, t1) (neg2, t2) (-1)
            in (negres, result + base :: coeffsres)
          else if result >= base then
            let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 1
            in (negres, result - base :: coeffsres)
          else
            let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 0
            in (negres, result :: coeffsres)
  in
  let (negres, coeffsres) =
        plus_with_carry (b1.neg, List.rev b1.coeffs)
                        (b2.neg, List.rev b2.coeffs)
                        0
  in {neg = negres; coeffs = stripzeroes (List.rev coeffsres)}


(*>* Problem 1.4 *>*)

let plus (b1 : bignum) (b2 : bignum) : bignum =
  (* if b1 < -b2, then b1 + b2 < 0, so evaluate -((-b2) + (-b2)) *)
  if less b1 (negate b2) then negate (plus_pos (negate b1) (negate b2))
  (* otherwise just use plus_pos function *)
  else plus_pos b1 b2
;;

let _ = assert((plus (fromInt 123) (fromInt 111)) = (fromInt 234))
let _ = assert((plus (fromInt 123) (fromInt (-123))) = (fromInt 0))
let _ = assert((plus (fromInt 123) (fromInt (-124))) = (fromInt (-1)))
let _ = assert((plus (fromInt (-124)) (fromInt 123)) = (fromInt (-1)))

(*>* Problem 1.5 *>*)

(* bignumTimesInt multiplies a bignum by an int n, assuming n > 0, using
   the grade school multiplication method, carrying over each result *)
let bignumTimesInt (b: bignum) (n: int) : bignum =
  let rec carryMult (coeffs: int list) (n:int) (c: int) : int list =
    match coeffs with
    (* if coeffs is empty we have reached the end of coeffs and must deal with
       the carried value c *)
    | [] -> 
      (* if c is 0 we do not need to carry the value over *)
      if c = 0 then []
      (* if c is not zero we return c as a list of itself *)
      else c::[]
    | hd::tl ->
      (* notCarried is the digit not carried over in multiplication
         eg 6 in 8 * 2 *)
      let notCarried = (hd * n + c) mod base in
        (* carried is the digit carried over in multiplication
           eg 1 in 8 * 2 *)
        let carried = (hd * n + c - notCarried) / base in
          (* we conn notCarried with the recursive call of carryMult
             on tl with n and the carried value *)
          notCarried :: carryMult tl n carried
  (* Here we have to reverse the b.coeffs initially before calling it within
     carryMult, because carryMult works on a reversed list of coeffs. The 
     resulting list is then reversed to put the coeffs in the proper order
     for bignum.  Because a bignum times a positive int has the same sign
     as the original bignum, we preserve b.neg with the 'with' statement. *)
  in {b with coeffs = List.rev (carryMult (List.rev b.coeffs) n 0)}
;;
(* this helper function adds the result of b1 times each coefficient of b2, 
   scaling for each coefficient's position in b2 *)
let addScaledBignumList (b: bignum list) : bignum =
  List.fold_left ~init: (fromInt 0) ~f:(fun x y -> 
                                        plus (bignumTimesInt x base) y) b
;;

let times (b1: bignum) (b2: bignum) : bignum =
  (* the List.map function multiplies b1 by each coefficient in b2, 
     returning a bignum list. This list is then added together with 
     addScaledBignumList, which takes into account each digits position, 
     scaling by base for each digit in b2 *)
  let b = addScaledBignumList (List.map ~f:(fun x -> bignumTimesInt b1 x) 
                                 b2.coeffs) in
  (* if b1 and b2 have opposite signs, the result is negative *)
  if b1.neg <> b2.neg then {b with neg = true}
  (* if b1 and b2 have the same sign, the result is positive *)
  else {b with neg = false}
;;

let _ = assert((times (fromInt 1234) (fromInt 1000)) = fromInt 1234000)
let _ = assert((times (fromInt 1234) (fromInt (-1000))) = fromInt (-1234000))
let _ = assert((times (fromInt (-1234)) (fromInt (-1000))) = fromInt 1234000)
let _ = assert((times (fromInt (-1234)) (fromInt 1000)) = fromInt (-1234000))
let _ = assert((times (fromInt 0) (fromInt 1234)) = fromInt 0)
let _ = assert((times (fromInt 1234) (fromInt 0)) = fromInt 0)

(* Returns a bignum representing b/n, where n is an integer less than base *)
let divsing (b : int list) (n : int) : int list * int =
  let rec divsing_rec (b : int list) (r : int) : int list * int =
    match b with
      | [] -> [], r
      | h :: t ->
          let dividend = r * base + h in
          let quot = dividend / n in
          let (q, r) = divsing_rec t (dividend-quot * n) in
          (quot :: q, r) in
    match b with
      | [] -> [], 0
      | [a] -> [a / n], a mod n
      | h1 :: h2 :: t -> if h1 < n then divsing_rec (h1 * base + h2 ::t) 0
        else divsing_rec b 0


(* Returns a pair (floor of b1/b2, b1 mod b2), both bignums *)
let divmod (b1 : bignum) (b2 : bignum): bignum * bignum =
  let rec divmod_rec m n (psum : bignum) : bignum * bignum =
    if less m n then (psum, m) else
      let mc = m.coeffs in
      let nc = n.coeffs in
      match nc with
        | [] -> failwith "Division by zero"
        | ns :: _ -> let (p, _) =
            if ns + 1 = base then
              (take_first mc (List.length mc - List.length nc), 0)
            else
              let den = ns + 1 in
              let num = take_first mc (List.length mc - List.length nc + 1)
              in divsing num den
          in
          let bp = clean {neg = false; coeffs = p} in
          let p2 = clean (if equal bp (fromInt 0) then fromInt 1 else bp) in
            divmod_rec (clean (plus m (negate (times n p2))))
                       (clean n)
                       (clean (plus psum p2))
  in
  divmod_rec (clean b1) (clean b2) (fromInt 0)


(**************************** Challenge 1: RSA ******************************)

(** Support code for RSA **)
(* Hint: each part of this problem can be implemented in approximately one
 * line of code. *)

(* Returns b to the power of e mod m *)
let rec expmod (b : bignum) (e : bignum) (m : bignum) : bignum =
  if equal e (fromInt 0) then fromInt 1
  else if equal e (fromInt 1) then
    snd (divmod (clean b) (clean m))
  else
    let (q, r) = divmod (clean e) (fromInt 2) in
    let res = expmod (clean b) q (clean m) in
    let (_, x) = divmod (times (times res res) (expmod (clean b) r (clean m)))
                        (clean m) in
    {neg = x.neg; coeffs = stripzeroes x.coeffs}

(* Returns b to the power of e *)
let rec exponent (b : bignum) (e : bignum) : bignum =
  if equal (clean e) (fromInt 0) then fromInt 1
  else if equal (clean e) (fromInt 1) then clean b
  else
    let (q, r) = divmod (clean e) (fromInt 2) in
    let res = exponent (clean b) q in
    let exp = (times (times res res) (exponent (clean b) r))
    in {neg = exp.neg; coeffs = stripzeroes exp.coeffs}

(* Returns true if n is prime, false otherwise. *)
let isPrime (n : bignum) : bool =
  let rec miller_rabin (k : int) (d : bignum) (s : int) : bool =
    if k < 0 then true else
    let rec square (r : int) (x : bignum) =
      if r >= s then false else
      let x = expmod x (fromInt 2) n in

        if equal x (fromInt 1) then false
        else if equal x (plus n (fromInt (-1))) then miller_rabin (k-1) d s
        else square (r + 1) x
    in
    let a = plus (randbignum (plus n (fromInt (-4)))) (fromInt 2) in
    let x = expmod a d n in
      if equal x (fromInt 1) || equal x (plus n (fromInt (-1))) then
        miller_rabin (k - 1) d s
      else square 1 x
  in
    (* Factor powers of 2 to return (d, s) such that n=(2^s)*d *)
  let rec factor (n : bignum) (s : int) =
    let (q, r) = divmod n (fromInt 2) in
      if equal r (fromInt 0) then factor q (s + 1) else (n, s)
  in
  let (_, r) = divmod n (fromInt 2) in
    if equal r (fromInt 0) then false else
      let (d, s) = factor (plus n (fromInt (-1))) 0 in
        miller_rabin 20 d s

(* Returns (s, t, g) such that g is gcd(m, d) and s*m + t*d = g *)
let rec euclid (m : bignum) (d : bignum) : bignum * bignum * bignum =
  if equal d (fromInt 0) then (fromInt 1, fromInt 0, m)
  else
    let (q, r) = divmod m d in
    let (s, t, g) = euclid d r in
      (clean t, clean (plus s (negate (times q t))), clean g)


(* Generate a random prime number between min and max-1 (inclusive) *)
let rec generateRandomPrime (min : bignum) (max: bignum) : bignum =
  let rand = plus (randbignum (plus max (negate min))) min in
    if isPrime rand then rand else generateRandomPrime min max



(* Pack a list of chars as a list of bignums, with m chars to a bignum. *)
let rec charsToBignums (lst : char list) (m : int) : bignum list =
  let rec encchars lst =
    match lst with
      | [] -> (fromInt 0)
      | c :: t -> clean (plus (times (encchars t) (fromInt 256))
                              (fromInt (Char.to_int c)))
  in
    match lst with
      | [] -> []
      | _ -> let (enclist, rest) = split lst m in
             encchars enclist :: charsToBignums rest m


(* Unpack a list of bignums into chars (reverse of charsToBignums) *)
let rec bignumsToChars (lst : bignum list) : char list =
  let rec decbignum b =
    if equal b (fromInt 0) then []
    else let (q, r) = divmod b (fromInt 256) in
      match toInt r with
        | None -> failwith "bignumsToChars: representation invariant broken"
        | Some ir -> Char.of_int_exn ir :: decbignum q
  in
    match lst with
      | [] -> []
      | b :: t -> decbignum b @ bignumsToChars t


(* Return the number of bytes required to represent an RSA modulus. *)
let bytesInKey (n : bignum) =
  Float.to_int (Float.of_int (List.length (stripzeroes n.coeffs) - 1)
                *. log10 (Float.of_int base) /. (log10 2. *. 8.))


 (* This helper function splits b into two bignums x1 and x0 where
    x = x1*b^(m2) + x0 using the split function, for Karatsuba algorithm *)

let splitBignumCoeffs (b: bignum) (m2: int) : bignum * bignum =
  let x = split (List.rev b.coeffs) m2 in
    ({b with coeffs = List.rev (snd x)}), ({b with coeffs = List.rev (fst x)})
;;

(* Returns a bignum representing b1*b2 *)
let rec times_faster (b1 : bignum) (b2 : bignum) : bignum =
  (* if b1 and b2 are less than 4 digits, use original times *)
  if List.length b1.coeffs <= 4 && List.length b2.coeffs <= 4 then
     times b1 b2
  else
  let b = (fromInt base) in
  (* m and m2 are used to determine where to split b1 and b2, m2 is
     also used when evaluating the final answer *)
  let m = Int.max (List.length b1.coeffs) (List.length b2.coeffs) in
  let m2 = m - (m / 2) in
  (* x1, x0 where b1 = x1*b^(m2) + x0 *)
  let x1, x0 = splitBignumCoeffs b1 m2 in
  (* y1, y0 where b2 = y1*b^(m2) + y0 *)
  let y1, y0 = splitBignumCoeffs b2 m2 in
  (* z2 = recursive call of x1*y1 *)
  let z2 = times_faster x1 y1 in
  (* z0 = recursive call of x0*y0 *)
  let z0 = times_faster x0 y0 in
  (* z1 = x1*y0 + x0*y1, using recursive call of times_faster *)
  let z1 = plus (times_faster x1 y0) (times_faster x0 y1) in
    (* x*y = z2*b^(2*m2) + z1*b^m2 + z0 *)
    plus (plus (times z2 (exponent b (fromInt (2 * m2)))) 
               (times z1 (exponent b  (fromInt m2)))) 
    z0
;;

let _ = assert((times_faster (fromInt Int.max_value) (fromInt Int.max_value)) 
                = times (fromInt Int.max_value) (fromInt Int.max_value))
let _ = assert((times_faster (fromInt 12345) (fromInt 6789)) 
                = times (fromInt 12345) (fromInt 6789))
let _ = assert((times_faster (fromInt (-12345)) (fromInt 6789))
                = times (fromInt (-12345)) (fromInt 6789))
let _ = assert((times_faster (fromInt 12345) (fromInt (-6789)))
                = times (fromInt 12345) (fromInt (-6789)))

