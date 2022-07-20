type t
(*@
model same : int
model same : bool list
*)


val silly_create : int -> int
(*@ t = silly_create c
requires c >= 5
*)

val silly_create : int -> int
(*@ t = silly_create c
requires c < 5
*)
