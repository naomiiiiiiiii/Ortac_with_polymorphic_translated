 open Translated
open Ast3
    open Ppxlib
(* open Builder *)

module S = Map.Make (String)

let unsupported_type s = raise (Failure ("unsupported type: " ^ s ))

let rec typ_of_type_ (ty: type_)  =
  let base_typ_of_string s =
    match s with
    | "int" -> Integer
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
let mk_qcheck (typ: Ast3.typ) : expression =
  let loc = !Ast_helper.default_loc in
  let rec mk_qcheck_help typ = 
  match typ with
  | Integer ->  [%expr frequency [(1, small_nat); (20, int)]]
  | String -> [%expr frequency [(1, small_string); (20, string)]]
  | Bool -> [%expr bool]
  | Unit -> [%expr unit]
  | List typ -> [%expr list [%e mk_qcheck_help typ]] in
  [%expr Gen.([%e mk_qcheck_help typ])]

(* [%expr [%e raise Div]] *)


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

let cmd (items: Translated.structure_item list) : Ast3.cmd =
 List.fold_right (fun s acc -> match s with
      | Value v when (v.arguments <> []) && (v.name <> "init_sut") ->
         (match (safe_add v.name (List.map (fun (arg: ocaml_var) ->
            mk_arg arg.name arg.label arg.type_)
                v.arguments) acc) with
         `Ok out -> out
         | `Duplicate key -> raise (Failure ("function declared twice: " ^ key)))
      | _ -> acc) items S.empty

let state items : Ast3.state  =
  match List.find_opt (fun s -> match s with
      | Type t when (String.equal t.name "t") -> true 
      | _ -> false) items with
  Some (Type t) -> List.map (fun (s, type_) -> (s, typ_of_type_ type_))
                     t.models
  | _ -> raise (Failure ("type t not declared; could not determine sut"))


let arb_cmd : cmd -> arb_cmd =
  S.map (fun args ->
      List.map (fun arg -> mk_qcheck arg.arg_type) args)


(*let payload_pattern name expr =
  let open Ast_pattern in
  pstr (pstr_value nonrecursive (value_binding ~pat:(pstring __) ~expr:__) ^:: nil)*)

(*this is gross
ret.field1 = 0*)

(*| [%type: [%t? t] list] ->
      [%expr
        fun lst ->
          "["
          ^ List.fold_left
              (fun acc s -> acc ^ [%e expr_of_type t] s ^ ";")
              "" lst
          ^ "]"]
is t a bound variable?
*)
    (* 
  let is_of_int fn = 
    match fn.pexp_desc with
    | Pexp_ident longident -> (longident.txt = Ldot (Ldot (Lident "Ortac_runtime", "Z"), "of_int"))
    | _ -> false in
  let get_ident exp = 
  let rec get_ident_exn (exp : expression) : string = match exp with
    | [%expr [%e? name].([%e? field]) ] -> Printf.sprintf "%s.%s" (get_ident_exn name)
                                          (get_ident_exn field)
    | [%expr [%e? fn] @@ [%e? real_exp]] when (is_of_int fn) -> (get_ident_exn real_exp)
    | _ -> match exp.pexp_desc with Pexp_ident longident -> (match longident.txt with
        | Lident s -> Some s
        | _ -> raise (Failure "no ident"))
    | _ -> raise (Failure "no ident")
    in
    try (Some (get_ident_exn exp)) with Failure _ -> None in
  let ident: string option = get_ident lhs in 
(match exp with
  [%expr: [%e? lhs] = [%e? rhs]] -> Option.map ident (fun s -> (s, rhs))
  | _ -> None ) *)

let get_sides  (exp: expression) : (string * expression) option  =
 let rec get_ident (exp: expression) : string option  =
   let rec get_ident_li (li : longident) = match li with
     | Lident s -> s
     | Ldot (li, s) -> Printf.sprintf "%s.%s" (get_ident_li li) s
     | Lapply (li1, li2) -> Printf.sprintf "(%s, %s)" (get_ident_li li1) (get_ident_li li2) in
    (* Don't actually need this because the models should never have type int
let is_of_int (fn : expression) : bool =
      match fn.pexp_desc with
      | Pexp_ident longident -> (longident.txt = Ldot (Ldot (Lident "Ortac_runtime", "Z"), "of_int"))
      | _ -> false in *)
    match exp.pexp_desc with
    | Pexp_ident longident -> (match longident.txt with
        | Lident s -> Some s
        | _ -> None)
    (*| Pexp_apply (fn, [(Nolabel, real_exp)]) when is_of_int fn -> raise (Failure "is_of_int in gospel spec")
    *)
    |  Pexp_field(exp, li) -> Option.map 
                                 (fun name -> Printf.sprintf "%s.%s" name (get_ident_li li.txt))
                                 (get_ident exp)
    | _ -> None in
  let is_equal (exp: expression) :bool = match exp.pexp_desc with
      | Pexp_ident longident -> (match longident.txt with
          | Lident "=" -> true
          | _ -> false)
      | _ -> false  in
  (match exp.pexp_desc with
  | Pexp_apply (fn, [(Nolabel, field_exp); (Nolabel, field_val)]) ->
    if (not (is_equal fn)) then None
    else Option.map  (fun s -> (s, field_val)) (get_ident field_exp)
  | _ -> None)


let get_lhs exp = Option.map fst (get_sides exp)

let get_rhs_exn exp = exp |> get_sides |> Option.get |> snd 


(*error check for if there are multiple init_sut start here
need to support old start here*) 
(*need some special processing of the post in order to get out the stuff
*)
(*1. get the name of the result
  then, for each field_name in state 
  2. look for result.field_name = out in the list of post
  3. return the out
  now i have a string * expression list with
  (field_name, expression it is initialized to )
*)
let init_state (items: Translated.structure_item list) (state: state): init_state =
  let init_state_ele (posts: term list) (field : string) (ret_name : string ) : expression =
    let field_name = Printf.sprintf "%s.%s" ret_name field in
    Printf.printf("field name is: %s\n%!") field_name;
   ( match List.find_opt (fun (term: term) -> match term.translation with
        Ok exp ->
        Printf.printf("lhs is: %s\n%!") (exp |> get_lhs |> Option.get);
        (get_lhs exp = Some field_name)
        | _ -> false 
      ) posts with (*found a term which sets the field name equal to something*)
    Some term -> (term.translation |> Result.get_ok |> get_rhs_exn)
    | None -> raise (Failure (Printf.sprintf "field %s undefined" field)))
    in
  match List.find_opt (fun item -> match item with
      | Value v when (String.equal v.name "init_sut") -> true 
      | _ -> false) items with
  Some (Value v) -> assert (List.length v.returns = 1);
  let ret_name = (List.hd v.returns).name in
  List.map (fun (field, _) -> (field, init_state_ele v.postconditions field ret_name)) state
  | _ -> raise (Failure "init_sut undefined; could not initialize")

