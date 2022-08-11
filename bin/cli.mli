(* Left empty on purpose. *)

type frontend = Default | Monolith | STM

val main : frontend -> string -> string option -> unit -> unit
