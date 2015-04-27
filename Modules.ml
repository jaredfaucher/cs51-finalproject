open Core.Std

module type SHAMIR =
sig
  type secret
  type poly
  type key
end

module type SHAMIR_ENCODE =
sig
  include SHAMIR
  (*type t*)
  val to_secret: int -> secret
  val gen_keys: secret -> int -> int -> key list
  val print_keys: key list -> unit
end

module type SHAMIR_DECODE =
sig
  include SHAMIR
  (*type t*)
  type lagrange_poly
  val to_key: (int * int) list -> key list
  val get_secret: key list -> int
end
