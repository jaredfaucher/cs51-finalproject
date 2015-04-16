open Core.std;;

module type SHAMIR_ENCRYPT =
sig
  type secret
  type threshold
  type num_participants
  type poly
  type key
  val gen_keys: secret -> threshold -> num_participants -> key list
end
