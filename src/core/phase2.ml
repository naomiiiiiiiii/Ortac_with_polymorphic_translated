 open Translated
open Ast3
    open Ppxlib
 open Builder 

module S = Map.Make (String)

let unsupported_type s = raise (Failure ("unsupported type: " ^ s ))

let value v = match v with Value v -> v | _ -> raise (Failure "not value")



(*start here take error out*)
let rec typ_of_type_ (error: string) (ty: type_)  =
  let base_typ_of_string s =
    match s with
    | "integer" ->  Integer
    | "int" ->  Int
    | "string" ->  String
    | "bool" -> Bool
    | "unit" -> Unit
    | _ -> unsupported_type s 
  in
  match ty.args with
  | [] -> base_typ_of_string ty.name
  | [arg] -> (match ty.name with
        "List" -> List (typ_of_type_ error arg)
      | _ -> unsupported_type ty.name)
  | _ -> raise (Failure "no type with multiple arguments supported")


(*
magic numbers*)
let mk_qcheck (typ: Ast3.typ) : expression =
  let loc = !Ast_helper.default_loc in
  let rec mk_qcheck_help typ = 
  match typ with
  | Int ->  [%expr frequency [(1, small_nat); (20, int)]]
  | Integer -> raise (Failure "cannot make generator for Gospel stdlib type Integer")
  | String -> [%expr frequency [(1, small_string); (20, string)]]
  | Bool -> [%expr bool]
  | Unit -> [%expr unit]
  | List typ -> [%expr list [%e mk_qcheck_help typ]]
  in
  [%expr Gen.([%e mk_qcheck_help typ])]

(* [%expr [%e raise Div]] *)
let mk_arg name label type_ : Ast3.arg =  {arg_name = name;
                                          arg_label = label;
                                          arg_type = typ_of_type_ name type_}

let safe_add (key : string) v (m : 'a S.t) = match S.find_opt key m with
  None -> `Ok (S.add key v m)
  | Some _ -> `Duplicate (key)

(*if these args are lists then i think you only get the word
"list" for type name which is not enough to make a generator.

print a drv example with a function that takes a bool list as argument.

if it does throw this out then you need to add params and argument fields to the type_ *)

let find_value items name =
  List.find_opt (fun item -> match item with
                    | Value v when ( v.name =
                                     name) -> true
                    | _ -> false) items |> Option.map value


let cmd (items: Translated.structure_item list) : Ast3.cmd =
  (*is v an stm command candidate *)
 let is_stmable (v : Translated.value) = (v.arguments <> []) && (v.name <> "init_sut") &&
                    (List.length v.arguments >= 1) && ((List.hd v.arguments).type_.name = "t") in
 let make_stmable (v: Translated.value) =
   ((List.hd v.arguments).name, {v with arguments = (List.tl v.arguments)}) in
 List.fold_right (fun item acc -> match item with
      (*need to require here that the first argument is t*)
      | Value v when (is_stmable v) ->
        let (tname, v) = make_stmable v in
         (match (safe_add v.name
                   {tname; args = (List.map (fun (arg: ocaml_var) ->
                        mk_arg arg.name arg.label arg.type_)
                        v.arguments)} acc) with
         |`Ok out -> out
         | `Duplicate key -> raise (Failure ("function declared twice: " ^ key)))
      | _ -> acc) items S.empty

let state items : Ast3.state  =
  match List.find_opt (fun s ->
      match s with
      | Type t when (String.equal t.name "t") -> true 
      | _ -> false) items with
  | Some (Type t) ->
    List.fold_right (fun (s, type_) acc ->
      match safe_add s (typ_of_type_ s type_) acc with
      |`Ok out -> out
      | `Duplicate key -> raise (Failure ("field declared twice: " ^ key))
    ) t.models S.empty 
  | _ -> raise (Failure ("type t not declared; could not determine sut"))


let arb_cmd : cmd -> arb_cmd =
  S.map (fun (cmd_ele: cmd_ele) ->
      List.map (fun arg -> mk_qcheck arg.arg_type) cmd_ele.args)


    (*  my attempt at using fancier patter matching, bad
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

(*what if instead of this garbage you took all the ensures that haven't been used already and put them in
the post condition*)
let rec contains_ident ident exp =
  let contains_ident = contains_ident ident in 
  match exp with
  | Pexp_ident li -> contains_ident_li ident li
  | Pexp_constant _ -> false
  | Pexp_let (_, vbs, exp) -> List.fold (fun vb ac -> contains_ident_vb vb || acc) vbs
                                (contains_ident exp)
  | Pexp_function cases -> 

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

let get_field_rhs ?(error = "Unknown") (equations: term list) (field: string)
    (prefix: string) : expression =
  let field = Printf.sprintf "%s.%s" prefix field in
  ( match List.find_opt (fun (term: term) -> match term.translation with
          Ok exp ->
          (* Printf.printf("lhs is: %s\n%!") (exp |> get_lhs |> Option.get); *)
          (get_lhs exp = Some field)
        | _ -> false 
      ) equations with (*found a term which sets the field name equal to something*)
      Some term -> (term.translation |> Result.get_ok |> get_rhs_exn)
    | None -> raise (Failure (Printf.sprintf "field %s undefined in %s" field error)))

(*init sut is the only one which returns a state
so to access the fields you do ret_name.fieldname

for all the others it's just arg_name.field name
wait lol**)
let make_next (cmd_item: Translated.value) (state: state) (prefix: string) =
  let loc = !Ast_helper.default_loc in (*start here ?? *)
  S.mapi (fun field _ -> if cmd_item.pure then [%expr s.([%e (evar field)]) ]
           (*s is a special variable*)
           else get_field_rhs ~error:cmd_item.name cmd_item.postconditions field prefix) state

let init_state (items: Translated.structure_item list) (state: state): init_state =
  match find_value items "init_sut" with
    Some cmd_item -> assert (List.length cmd_item.returns = 1);
    make_next cmd_item state (List.hd cmd_item.returns).name
  | None -> raise (Failure "init_sut undefined; could not initialize")

let translate_checks = List.map (fun check -> check.translations |> Result.get_ok |> fst)

let next_state items (cmds: cmd) state : next_state =
  S.mapi (fun cmd (cmd_ele : cmd_ele) ->
      let args = cmd_ele.args in
      let cmd_item = (match (find_value items cmd) with
          None -> raise (Failure ("could not find " ^ cmd))
          | Some cmd_item -> cmd_item)
      in
      let pres : expression list =
        (List.map (fun (pre: Translated.term) -> pre.translation |> Result.get_ok) cmd_item.preconditions) @
      (translate_checks cmd_item.checks) in
        (*silly processing to get the check*)
      let next = make_next cmd_item state cmd_ele.tname in
      {args; pres; next})
    cmds

(*if it doesnt say pure assume it can raise*)
let run items cmds = S.mapi
    (fun cmd args ->
    let cmd_item = find_value items cmd |> Option.get in (args, cmd_item.pure))
    cmds

(*would be nice if you could warn the user if some postconds are not translated
start here*)
let postcond items cmds state : postcond =
  S.mapi
(fun cmd cmd_data ->
  let cmd_item = Option.get (find_value items cmd) in
  let args = cmd_data.args in
  let checks = translate_checks cmd_item.checks in
  let raises = [] in (*start here*)
 let postcond = List.filter (contains_ident ret_name) (*start here*)
     (List.map (fun t -> t.translation) cmd_item.postconditions) in
   (*want the whole equals here
                                                           not just the rhs,
                                                           probably shouldnt use make_next*)
     (*need a conjunction of all the ensures with the result name on either side of the equals*)
{args; checks; raises; postcond }
)
cmds 

let stm (driver : Drv.t) : Ast3.stm  =
  let items = driver.translations in 
  let cmd = cmd items in
  let state = state items in
  let arb_cmd = arb_cmd cmd in
  let init_state = init_state items state in
  let next_state = next_state items cmd state in
  let run = run items cmd in
  let postcond = postcond items cmd state in
  {module_name = driver.module_name; cmd; state; arb_cmd; init_state; next_state; run; postcond}



