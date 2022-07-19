(* Left empty on purpose. *)

type frontend = Default | Monolith

val main : frontend -> string -> string option -> unit -> unit
