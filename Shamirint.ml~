open Core.Std;;

type poly = int list;;
type key = int * int;;

(* Encoding functions *)
let rec gen_poly (s:int) (t:int) : poly =
  match t with
  | 1 -> [s]
  | _ -> 
    Random.self_init();
    let r = (Random.int s) in
      r::(gen_poly s (t - 1));;

let eval_poly (x: int) (poly: poly) : int =
  let rec helper (x: int) (poly: poly) : int list =
    match poly with
    | [] -> []
    | hd::tl ->
      hd::(helper x (List.map ~f:(fun a -> x*a) tl))
  in
  List.fold_left (helper x poly) ~f:(+) ~init:0;;
  
let gen_keys (s:int) (t: int) (n: int): key list =
  let rec helper (n:int) (poly: poly) : key list =
  match n with
  | 0 -> []
  | _ ->
    (n, (eval_poly n poly))::(helper (n-1) poly)
  in
  let poly = List.rev (gen_poly s t) in
    helper n poly;;

(* decoding functions *)
