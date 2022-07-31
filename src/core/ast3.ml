module W = Warnings
module S = Map.Make (String)

open Ppxlib
(*open Sexplib.Std *)


(*the terms in DVT already have ortac stuff added to them. what you really want is the terms from the original spec which are
the same type as these terms.
DECISION. DRV.T IS TOO FAR DOWN. NEED TO START FROM TAST and generate a
1) Translated.structure item list (but with the original terms)
2) type? think about this later. the type_ in a Drv.t doesnt store arguments, aliasing, etc.
  but i don't think i need any of that? maybe for polymorphic type if you have a polymorphic field.

modify file translate.ml to turn a TAST into a Translated.structure item list.
from there turn into this.
*)

(*all the types that are supported for state fields *)

(*need to change this to be the gospel ints*)

(*150
218
270
242

  vs

  500
  270
  
  333
*)

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
  (*the fields of type t should not use the int type but the arguments
    and return type of create can use the int type
    the fields of type t are the only ones that you ever have to pull out of the ensures
    so you dont need to put back the is_of_int check in get_sides
    but do add an int constructor above *)

(*
type term = {
  txt : string;
  translation : ((expression, W.t) result [@sexp.opaque]);
}
[@@deriving sexp_of] *)

type ocaml_var = {name : string; label : arg_label; typ: typ}

(*maps the name of the fn to its argument types and return types, and also the name of the
  special first argument which is always of type t
  (not included in the cmd type, etc)
*)
(*start here you should use the targ_name instead of s for the old state name*)
(*ret is large list when tuple. what name does ortac use for the whole tuple?
none, gospel makes you deconstruct the tuple
*)
type cmd_ele = {targ_name: string; args: ocaml_var list; ret: ocaml_var list }
type cmd= cmd_ele S.t

(*will get this from the models of t (sut)
since the other types are stored as strings might as well
i think the types of the models may be thrown out by the time you get to the tast ...
definitely thrown out by the time you get to drv?
field name * field type
  really does not support polymorphism, need a much better type for types than strings
*)
type state = typ S.t

(*type sut = NOT NECESSARY, just do module_name.t *)

type arb_cmd = (expression list) S.t
(*pair up cmd constructor with Qcheck generators for args.
if arg list is empty then do Gen.return
*)

(*to get init_sut assume method in the gospel file called create? which describes how the
fields in the state are set*)
(*assume for now <5 args or whatever the highest map is *)

type init_state = expression S.t
  (* <- just a tuple of value. get these values from the ensures of create in gospel which in the drv.t is a term list. you have to go into the term to find the state_element_n = __expression__.
     ensures condition.*)

(*ensures fst = 5*)



(*get this from the spec of the function for that state
  if <all the requires and all the checks> then <correct ending state> else <same state as passed in
  where correct ending state is computed as
  <search through the ensures for state_entry_n = ____> <- START HERE where is this put exactly? what does this look like?
  right now packaged as
  for each function, <a list of the arguments>, <A list of all the requires and checks>,
  <a map of field name to the thing it is assigned to>
*)
type next_state_case = {args: ocaml_var list; pres: expression list; next: expression S.t} 
type next_state = next_state_case S.t 

(*command name -> arguments, can raise exn*)
type run = (ocaml_var list * bool) S.t


(*all the requires for a simple conjoining*)
type precond = expression list S.t 


(*iff checks then raise invalid argument
iff a raises then raise that Exn (dont support this for right now)
Otherwise need to look in the ensures for all conditions to do with result _op _ _rhs_
for each fn
  (the arguments for the pattern matching, the return string for pattern matching,
  all the checks,
  the result in the ensures)
*)
type postcond_case =
  {args: ocaml_var list;
   ret: ocaml_var list;
   checks: expression list;
   raises: (string * expression) list; (*? is this the right type*)
   postcond: expression list; (*the expressions that go in here are all the ensures
                              that have not already been used figuring out the state
                              the ones that have already been used getting to this
                              point are the ones to do with state.field =
                              everything else should be in scope?
if you were to write
                                val get : t -> int -> int
                                out = get s i
                                ensures out = s.field1
                                all the args are in scope and above is correct syntax for
                                the state.
                                i think gospel type checking should be sufficient to ensure
                                that all the remaining preconditions are well typed.
so, which functions "use" post conditions?
                                just next_state. 

                              *) }
  (*if you wrote
val get : t -> int -> int
    out = get s i
    s.field1 = out
    this would break the next_state function generation because out is not in scope
    need to check the rhs of all the next states does not use the return name.
    ^ no way to get around writing the contains_ident function.
    can i just run the ocaml typechecker on the = expression with out outside of scope?
    how does the gospel typechecker check scoping <- LOOK IN HERE START HERE

    that said all the ensures that aren't already used SHOULD go in the post condition
    so i think this approach is still a good idea.
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
they can only refer to the FIELDS of sut. start here check this. *)


(*where to put the requires? in the precondition? *)
