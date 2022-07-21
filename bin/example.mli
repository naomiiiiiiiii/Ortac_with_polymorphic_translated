type t
(*@
model field1 : integer list
model field1 : integer list
*)

(*how init sut must be written *)

val init_sut : unit -> t
(*@
whatever = init_sut () 
ensures whatever.field1 = 42::[]
*)

