open Core.std;;

module type SHAMIR_ENCRYPT =
sig
  type secret
  val threshold: int
  val num_participants: int
  val gen_keys: secret -> threshold -> num_participants -> key list
end


 
