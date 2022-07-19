type 'a t
(*@ invariant 1 < 0 *)

exception Silly


val create : int -> int -> 'a t * int
(*@ (t, silly2) = create silly c
  checks silly >= 5 (*op_char_1*)
  requires c > 0
    ensures silly2 < 10
  raises Silly -> silly = 6*)
