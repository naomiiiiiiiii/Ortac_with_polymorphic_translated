type t
(*@
model field1 : integer list
model field2 : bool
*)

(*how init sut must be written *)

val init_sut : unit -> t
(*@
whatever = init_sut () 
ensures whatever.field1 = 42::[]
ensures whatever.field2 = true
*)

