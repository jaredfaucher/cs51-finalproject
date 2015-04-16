open Core.std;;

module type SHAMIR_DECRYPT =
sig
  type key
  type secret
  type poly
  type lagrange_poly
  val get_secret: key list -> secret
end
