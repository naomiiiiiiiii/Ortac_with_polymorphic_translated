module W = Warnings
module S = Map.Make (String)
open Ppxlib
module Ident = Gospel.Identifier.Ident


(*Respresentation that comes after Drv.t*)
                 


type typ =
  | Int
  | Integer
  | String
  | Bool
  | Unit
  | List of typ

let get_typ_args t =
  match t with
  | List a -> [a]
  | Int | Integer | String | Bool| Unit -> []


type ocaml_var = {name : string; label : arg_label; typ: typ}


(*maps the name of the fn to its argument types and return types,
  and also the name of the
  special first argument which is always of type t(not included in the cmd type)
*)
type cmd_ele = {targ_name: string; args: ocaml_var list; ret: ocaml_var list; pure:bool}
type cmd= cmd_ele S.t

(*a record with field names taken from the models of type t*)
type state = typ S.t

(*cmd constructor -> list of Qcheck generators for args
*)
type arb_cmd = (expression list) S.t

(*to get init_sut assume method in the gospel file called create? which describes how the
fields in the state are set*)

(* maps each state field name to its initial value
determined by the post conditions of special function init_sut*)
type init_state = expression S.t


(*cmd constructor -> {requires and checks; state field name -> new value}
*)
type next_state_case = { pres: expression list; next: expression S.t} 
type next_state = next_state_case S.t 

(*command name -> {what it returns, is it pure} *)
type run = (ocaml_var list * bool) S.t


(*cmd_constr -> all its requires*)
type precond = expression list S.t 



(*
not sure if this is a good idea or not
start here make each of the types self contained so you dont need to take in the cmd as arg
in phase 3 and use stateful finds everywhere
to do this you would need polymorphic records in some of the case generating fns
*)


type xpost = {
  name : string;
  args : int;
  translation : cases; 
}

type postcond_case =
  {
   checks: expression list;
   raises: xpost list; 
   ensures: expression list; }
  (*if you wrote
val get : t -> int -> int
    out = get s i
    s.field1 = out
    this would break the next_state function generation because out is not in scope
    need to check the rhs of all the next states does not use the return name.
    this should go in the postcondition, not in next state. but how to know that?

    ^ no way to get around writing the contains_ident function.
    can i just run the ocaml typechecker on the = expression with out outside of scope?
    how does the gospel typechecker check scoping <- LOOK IN HERE START HERE start here this is serious

  *)
type postcond = postcond_case S.t

type stm = {module_name : string;
            cmd: cmd;
            state : state;
            arb_cmd : arb_cmd;
            init_state: init_state;
            next_state: next_state;
            run: run;
            precond : precond;
            postcond: postcond
           }

(*assumptions:
  1. the system under test is called 't' in the gospel file
  2. all of the functions in the file have types of the form
  t -> basic_type ... -> basic_type where basic_type is a type
  with a generator in QCheck
  3. there is a function init_sut : () -> t
4. none of the conditions can refer to sut : t directly.
they can only refer to the FIELDS of sut. start here check this.
5. the setting of the next state is written with the state field on the LHS.

  this is to allow the user to write out = sut.field1 to mean
  the output should equal the newly updated field of the sut,
  rather than assigning the newly updated sut field1 to be equal to the output,
  which is not well scoped
*)


