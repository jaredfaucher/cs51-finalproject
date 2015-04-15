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

let add_polys (x:poly) (y:poly) : poly =
  let rec helper (x:poly) (y:poly) (acc:poly) : poly =
    match (x, y) with
    | ([],[]) -> acc
    | ([], yh::yt) -> helper [] yt (yh::acc)
    | (xh::xt, []) -> helper xt [] (xh::acc)
    | (xh::xt, yh::yt) ->
      helper xt yt ((xh+yh)::acc)
  in helper x y []
;;

let mult_x_a_poly (a: int) (poly: poly) : poly =
  let x_half = poly @ [0] in
  let a_half = List.map ~f:(fun x -> a * x) poly in
  add_polys x_half a_half
;;
