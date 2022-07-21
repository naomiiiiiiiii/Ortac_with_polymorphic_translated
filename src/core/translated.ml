module W = Warnings
open Ppxlib
open Sexplib.Std

  type mutability =
    | Unknown
    | Immutable
    | Mutable
    | Dependant of (mutability list -> mutability)
  [@@deriving sexp]

type res = (expression, W.t) result

let sexp_of_res res : Sexplib.Sexp.t = 
  match res with
    Ok exp -> Sexplib__Sexp.Atom (Ppxlib_ast.Pprintast.string_of_expression exp)
  | Error _ -> Sexplib__Sexp.Atom ("error") 

(* 
    Fmt.pf Fmt.stdout "TERM ISSSSS: %a@." Ppxlib_ast.Pprintast.expression exp
  | Error _ -> Printf.printf "error\n%!" *)

type term = {
  txt : string;
  loc : (Location.t [@sexp.opaque]);
  translation : res  ;
}
[@@deriving sexp_of]


type check = {
  txt : string;
  loc : (Location.t [@sexp.opaque]);
  translations : ((expression * expression, W.t) result [@sexp.opaque]);
  (** The first expression ensures the condition holds, the second one
     contains the negative test (used when [Invalid_argument] is raised
to make sure that the checks actually was violated
      ) *)
}
[@@deriving sexp_of]

type invariant = {
  txt : string;
  loc : (Location.t [@sexp.opaque]);
  translation : ((string * structure_item, W.t) result [@sexp.opaque]);
}
[@@deriving sexp_of]

type transtype = (string * structure_item, W.t) result

let sexp_of_transtype t = match t with
    Ok (s, _) -> sexp_of_string s
  | Error _ ->  sexp_of_string "warning"

let transtype_of_sexp _ =
  Error (W.Unsupported "line 33 translated.ml", W.fake_loc)

(*need to mark whether it's a variable or not to support polymorphism
can't confuse the variable alpha with a type named alpha which is
what happens currently in translation*)
(*say 'a list is a list applied to a type 'a which is a type variable*)


(*type 'a t = 'a list
maybe in making the driver I remove all polymorphic
variables and replace them with int*)
type type_ =
  { name : string;
  (*ty : Gospel.Ttypes.ty option; option for the gospel stdlib types*)
    args : type_ list ;
  loc : (Location.t [@sexp.opaque]) ;
  mutable_ : mutability;
  ghost : Gospel.Tast.ghost;
  models : (string * type_) list;
  (*name and __ something else. need to change this to include type.
    support this later. *)
  invariants : invariant list;
  equality : ((expression, W.t) result [@sexp.opaque]);
  comparison : ((expression, W.t) result [@sexp.opaque]);
  copy : ((expression, W.t) result [@sexp.opaque]);
}[@@deriving sexp_of]


(*type t = int list *)
let type_ ~name ~loc ~mutable_ ~ghost =
 {
    name;
    args = [];
    loc;
    mutable_;
    ghost;
    models = []; (*drops all models and invariants here*)
    invariants = [];
    equality = Error (W.Unsupported "equality", loc);
    comparison = Error (W.Unsupported "comparison", loc);
    copy = Error (W.Unsupported "copy", loc);
  }

type ocaml_var = {
  name : string;
  label : (arg_label [@sexp.opaque]); (*probably useful to know*)
  type_ : type_; (**)
  modified : bool; (*stm cant test this*)
  consumed : bool; (*stm can't test this*)
}
[@@deriving sexp_of]

(*how to use ppx lib for this?*)
(* let print_cases (c : case) = [%stri [%c c]] *)

type xpost = {
  exn : string;
  args : int;
  translation : ((cases, W.t list) result [@sexp.opaque]); (*fix this*)
}
[@@deriving sexp_of]

(*functions are put in here*)
type value = {
  name : string;
  loc : (Location.t [@sexp.opaque]);
  arguments : ocaml_var list;
  (*name and type of the args, only need the types
    but the types are represented as only strings and you need the
    TYPE type (ty)
    for example the string for 'bool list' is just list
  *)
  returns : ocaml_var list; (*name and type of the returns*)
  register_name : string; (*dont need this, it's to do with the error wrappers*)
  ghost : Gospel.Tast.ghost;
  (*probably have to throw out any of the ones that say ghost*)
  pure : bool; (*useful for next_state*)
  checks : check list; (*useful for run*) 
  preconditions : (term list );
  (*this has as text the original conds written in the gospel file
    and also some ppx expressions which already have ortac stuff inserted
    by translate.ml
    *) 
  postconditions : (term list);
  xpostconditions : xpost list; (*cases are in here*)
}
[@@deriving sexp_of]

(*the string * *)

let value ~name ~loc ~arguments ~returns ~register_name ~ghost ~pure =
  {
    name;
    loc;
    arguments;
    returns;
    register_name;
    ghost;
    pure;
    checks = [];
    preconditions = [];
    postconditions = [];
    xpostconditions = [];
  }

type constant = {
  name : string;
  loc : Location.t;
  type_ : type_;
  register_name : string;
  ghost : Gospel.Tast.ghost;
  checks : term list;
  invariants : expression list;
}

(*lose all checks and invariants (how can a constant have a check?)
these checks are the post conditions of a constant
  (which doesn't make a ton of sense either)
*)
let constant ~name ~loc ~type_ ~register_name ~ghost =
  { name; loc; type_; register_name; ghost; checks = []; invariants = [] }

type axiom = {
  name : string;
  loc : (Location.t [@sexp.opaque]);
  register_name : string; (*dont need this*)
  definition : term;
}[@@deriving sexp_of]

type function_ = {
  name : string;
  loc : (Location.t [@sexp.opaque]);
  rec_ : bool;
  arguments : ocaml_var list;
  definition : term option;
}
[@@deriving sexp_of]

type structure_item =
  | Type of type_ (*literally never look at these except to get the fields for the state*)
  | Value of value (*this one (real functions are put in here)*)
 | Constant of (constant [@sexp.opaque ]) (*nope*)
  | Function of function_ (*not sure what goes in here*)
  | Predicate of function_ (*nope*)
  | Axiom of axiom (*nope*)
      [@@deriving sexp_of]



