open Core.std;;
open Big_int;;

type poly = big_int list;;
type key = big_int * big_int;;

(* encoding functions *)

(* Generates random big_int polynomial based on the secret
 * s and the threshold value t. f(x) = 123 + 4x + 5x^2 is
 * represented as [123; 4; 5] *)
let gen_poly (s: big_int) (t: big_int) : poly =
  let rec helper (s: big_int) (t: big_int) : poly =
    match t with
    | unit_big_int -> [s]
    | _ ->
      Random.self_init();
      let r = big_int_of_int (Random.int Int.max_value) in
      r::(helper s (pred_big_int t))
  in List.rev (helper s t)
;;

(* Evaluates a polynomial at x *)
let eval_poly (x: big_int) (poly: poly) : big_int =
  let rec helper (x: big_int) (poly: poly) : big_int list =
    match poly with
    | [] -> []
    | hd::tl ->
      hd::(helper x (List.map ~f:(fun a -> mult_big_int x a) tl))
  in
  List.fold_left (helper x poly) 
    ~f:(fun x y -> add_big_int x y) ~init:zero_big_int
;;

(* Generates a list of keys based on s, t and n *)
let gen_keys (s: big_int) (t: big_int) (n: big_int) : key list =
  let rec helper (n: big_int) (poly: poly) : key list =
  match n with
  | zero_big_int -> []
  | _ ->
    (int_of_big_int n, (eval_poly n poly))::(helper (pred_big_int n) poly)
  in
  let poly = gen_poly s t in
  helper n poly
;;

(* decoding functions *)
