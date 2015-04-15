open Core.Std

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
  let keys = gen_keys secret threshold num_participants in
  List.map (List.rev keys) ~f:(fun x ->
    Printf.printf "(%i, %i)\n" (fst x) (snd x))
;;

main ();;
