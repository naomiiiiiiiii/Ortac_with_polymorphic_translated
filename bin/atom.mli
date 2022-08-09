type t
(*@ model contents: integer *)

val init_sut: unit -> t
(*@ ret = init_sut ()
 ensures ret.contents = 0 *)


val get : t -> int
(*@
v = get atom
pure
ensures v = atom.contents
*)

val set : t -> int -> unit 
(*@ () = set atom s
checks s > 0
modifies atom.contents
ensures atom.contents = s 
*)

val exchange : t -> int -> int
(*@ out = exchange atom i
requires i > 42
modifies atom.contents
ensures atom.contents = i
*) 

val compare_and_set: t -> int -> int -> bool
(*@ out = compare_and_set atom seen v
requires seen + v = 30
modifies atom.contents
ensures atom.contents = v
(*this is a bad spec but ortac does not support if clauses right now *) 
*)

val five_args : t -> int list -> int list list -> int -> int -> int -> bool
(*@ out = five_args atom i two three four five
pure *)

(*val cause_error : t -> int
(*@ x = cause_error i*)
(*leave out what happens to contents*)
*)

(*
val fetch_and_add: t -> int -> int 

val incr : t -> unit

val decr: t -> unit *)
