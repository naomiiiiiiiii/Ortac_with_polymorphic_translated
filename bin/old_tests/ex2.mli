type 'a t
(** The type for containers. *)

val create : int -> int -> 'a t * int
(*@ (t, silly2) = create silly c
  checks silly >= 5
    ensures silly2 < 10
  *)

val is_empty : 'a t -> bool
(** [is_empty t] is [true] iff [t] contains no elements. *)
(*@ b = is_empty t
  pure
  requires 4 > 5 
  requires 5 > 4 (*has to be the second requires for the mistake to happen*)
    ensures 5 > 4*)


exception Silly
