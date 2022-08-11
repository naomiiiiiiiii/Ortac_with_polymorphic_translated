type t
(*@ model contents: integer *)

val init_sut: unit -> t
(*@ ret = init_sut ()
 ensures ret.contents = 42 *)


val get : t -> int
(*@
v = get atom
pure
ensures v = atom.contents
*)

val set : t -> int -> unit 
(*@ () = set atom s
modifies atom.contents
ensures atom.contents = s 
*)

val exchange : t -> int -> int
(*@ out = exchange atom i
modifies atom.contents
ensures atom.contents = i
*) 

(*val compare_and_set: t -> int -> int -> bool
(*@ out = compare_and_set atom seen v
modifies atom.contents
ensures  atom.contents = v *)*) 
(*this is a bad spec but ortac does not support if clauses right now *) 



(*
val fetch_and_add: t -> int -> int 

val incr : t -> unit

val decr: t -> unit *)
