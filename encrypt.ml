open Core.std;;

module type SHAMIR_ENCRYPT =
sig
  type poly = int list
  type key = int * int
  val gen_keys: secret -> threshold -> num_participants -> key list
end


 
