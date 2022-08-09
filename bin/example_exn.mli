type t
(*@ model contents: integer *)

exception Exn1

exception Exn2 of int
exception Exn3 of int

val init_sut : unit -> t
(*@ ret = init_sut ()
ensures ret.contents = 0*)

val fn_one: t -> int -> int
(*@
out = fn_one t_arg arg1
requires t_arg.contents > 0 
checks t_arg.contents < 5
raises Exn1 -> arg1 = 1
raises Exn2 i
raises Exn3 i -> 42=42 | Exn3 _ -> 54 =54 
(*here if the test case is used then the _ case is never entered which is bad*)
ensures t_arg.contents = 1
ensures out <= 800
*)

(*
raises Exn2 s -> s = "5"
xposts are 
(
Exn2,
  ( start of list 
    ( start of first tuple 
      (pattern s)

    (term s = 5)

    )))*)

