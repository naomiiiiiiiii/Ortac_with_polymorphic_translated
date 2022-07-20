 open Translated
open Ast3

module S = Map.Make (String)

let unsupported_type s = raise (Failure ("unsupported type: " ^ s ))

let rec typ_of_type_ (ty: type_)  =
  let base_typ_of_string s =
    match s with
    | "int" -> Int
    | "string" -> String
    | "bool" -> Bool
    | "unit" -> Unit
    | _ -> unsupported_type s 
  in
  match ty.args with
  | [] -> base_typ_of_string ty.name
  | [arg] -> (match ty.name with
    "List" -> List (typ_of_type_ arg)
    | _ -> unsupported_type ty.name)
  | _ -> raise (Failure "no type with multiple arguments supported")

(*ask Jan is it the best idea to do strings here
also magic numbers for the odds*)
let mk_qcheck (typ: Ast3.typ) =
  let rec mk_qcheck_help typ = 
  match typ with
  Int ->  "(frequency [(1, small_nat); (20, int)])"
  | String -> "(frequency [(1, small_string); (20, string)])"
  | Bool -> "bool"
  | Unit -> "unit"
  | List typ -> Printf.sprintf "(list %s)" (mk_qcheck_help typ) in
  Printf.sprintf "Gen.%s" (mk_qcheck_help typ)

let mk_arg name label type_ : Ast3.arg = {arg_name = name;
                                          arg_label = label;
                                          arg_type = typ_of_type_ type_}

let safe_add (key : string) v (m : 'a S.t) = match S.find_opt key m with
  None -> `Ok (S.add key v m)
  | Some _ -> `Duplicate (key)

(*if these args are lists then i think you only get the word
"list" for type name which is not enough to make a generator.

print a drv example with a function that takes a bool list as argument.

if it does throw this out then you need to add params and argument fields to the type_ *)

let cmd items : Ast3.cmd =
 List.fold_right (fun s acc -> match s with
      | Value v when (v.arguments <> []) ->
         (match (safe_add v.name (List.map (fun (arg: ocaml_var) ->
            mk_arg arg.name arg.label arg.type_)
                v.arguments) acc) with
         `Ok out -> out
         | `Duplicate key -> raise (Failure ("function declared twice: " ^ key)))
      | _ -> acc) items S.empty


(*start here need to change the type_.models to include ttypes.ty

first check -- do they have this anywhere? for the model?
  in the tast? no
  in the uast? *)
   (*
let state items : Ast3.state  =
  match List.find_opt (fun s -> match s with
      | Type t when (String.equal t.name "t") -> true 
      | _ -> false) items with
  Some (Type t) -> 

  | _ -> raise (Failure ("type t not declared; could not determine sut"))


let arb_cmd =
  S.map (fun args ->
      List.map args (fun arg -> mk_qcheck arg.arg_type))
*)
