(*cannot refer to values of type t directly in conditions. 
can only refer to their fields.*)

(*error produced when ignoring this assumption*)

type t


val get : t -> int
(*@
pure
*)

val set : t -> int -> unit
(*@ () = set atom s
requires get atom  = 5 
(*this requires must be checked in the precond, 
but the sut is not available in the precond. only the state. 
the STM test driver is only "supposed" to interact with the sut in run.
all conditions apply to the state. *)
*)
