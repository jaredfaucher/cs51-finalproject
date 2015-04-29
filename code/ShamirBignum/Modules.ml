open Core.Std
open BigNum

(* Assumes the number of participants and/or the threshold is within
 * integer bounds*)

 (* Module contains basic type identifiers to be used in encryption and
  * decryption algorithms *)
module type SHAMIR =
sig
  type secret
  type poly
  type key
end

 (* Module contains basic type identifiers to be used in encryption
  * algorithm *)
module type SHAMIR_ENCODE =
sig
  include SHAMIR
  (*type t*)
  val to_secret: bignum -> secret
  val gen_keys: secret -> int -> int -> key list
  val print_keys: key list -> unit
end

 (* Module contains basic type identifiers to be used in dencryption
  * algorithm *)
module type SHAMIR_DECODE =
sig
  include SHAMIR
  (*type t*)
  type lagrange_poly
  val to_key: (int * bignum) list -> key list
  val get_secret: key list -> bignum
end
