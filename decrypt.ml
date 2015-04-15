open Core.std

module type SHAMIR_DECRYPT =
sig
  type key = (int * int)
  type secret = int
  type lagrange_poly = int list
  val gen_lagrange_polys : key list -> lagrange_poly list
  val gen_secret: lagrange_poly list -> secret
end
