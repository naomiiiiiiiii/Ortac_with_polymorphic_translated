module W = Warnings
open Ppxlib
open Builder
open Ast3
module F = Failure
module T = Translation
module S = Map.Make (String)
module ISet = Set.Make (Int)


let rec splitn n l = if n = 0 then ([], l) else
    match l with
    [] -> ([], [])
    | x::xs -> let (left, right) = splitn (n-1) xs in (x::left, right)


(*ast3 helpers *)
let rec typ_to_str ?(args = false) ?(paren_args = false) ?(capitalize = false) t =
let out =  match t with
  | Integer -> "Ortac_runtime.Z.t"
  | Int -> "int"
  | String -> "string"
  | Bool -> "bool"
  | Unit -> "unit"
  | List typ -> if args then
      let unparened =  "list " ^ (typ_to_str ~args:true ~paren_args:true ~capitalize:capitalize typ) in
      if paren_args then "(" ^ unparened ^ ")" else unparened 
    else "list" in
if capitalize then String.capitalize_ascii out else out 

let rec typ_to_core_type (t : Ast3.typ) : core_type =
  ptyp_constr (t |> typ_to_str |> lident)  (List.map typ_to_core_type (Ast3.get_typ_args t))
let mk_core_typ (arg : Ast3.ocaml_var) : core_type = typ_to_core_type arg.typ
(* [%type int][@subst let t : string = (type_to_string t)] *)
(* [%type [%t typ_to_string arg.typ]] *)

(*ask jan not allowing tuple types because stm has no 'a ty_show for tuples
  let typ_to_qtyp t = *)


(*ppx helpers*)

(*i dont think you need this, i think lident does this already*)
let lident_dot s =
  let names = String.split_on_char '.' s in
  assert (List.length names >= 1);
  List.fold_left (fun acc name ->
match acc with
  None -> Some (Lident name)
  | Some li -> Some (Ldot (li, name))) None names |> Option.get |> noloc

(*let loc : Location.t =
  let fake_pos : Lexing.position = {pos_fname = "fake"; pos_lnum = (-1); pos_bol = (-1); pos_cnum = (-1) } in
  {loc_start = fake_pos; loc_end = fake_pos; loc_ghost = true } *)

let show_attribute : attribute =
  let loc = !Ast_helper.default_loc in 
  {attr_name = (noloc "deriving");
                         attr_payload =
                           PStr
                             [{pstr_desc =
                                 Pstr_eval
                                   ({pexp_desc =
                                       Pexp_apply
                                         ({pexp_desc = Pexp_ident {txt = Lident "show"; loc};
                                           pexp_loc_stack = []; pexp_attributes = []; pexp_loc = loc},
                                          [(Nolabel,
                                            {pexp_desc =
                                               Pexp_record
                                                 ([({txt = Lident "with_path"; loc},
                                                    {pexp_desc =
                                                       Pexp_construct ({txt = Lident "false"; loc}, None);
                                                     pexp_loc_stack = []; pexp_attributes = []; pexp_loc = loc})],
                                                  None);
                                             pexp_loc_stack = []; pexp_attributes = []; pexp_loc = loc})]);
                                     pexp_loc_stack = []; pexp_attributes = []; pexp_loc = loc},
                                    []);
                               pstr_loc = loc;
                              }];
                                    attr_loc = loc
                                   } 

let mk_fn (arg_names : string list list) (body: expression) : expression =
  List.fold_right (fun arg_tuple acc ->
      pexp_fun Nolabel None (ppat_tuple (List.map pvar arg_tuple)) acc) arg_names body

let mk_fn_single (args : string list) =
  mk_fn (List.map (fun x -> [x]) args)

let mk_arg_names n prefix = List.init n (fun i -> prefix^ (Int.to_string i))
let mk_app (fn : expression) (args : expression list) = pexp_apply fn
    (List.map (fun arg -> (Nolabel, arg)) args)

let conjoin (exps: expression list) =  match exps with
  | [] -> [%expr true]
  | r::rs -> List.fold_right (fun r acc ->
           [%expr [%e r] && [%e acc]]) rs r

let mk_record ?(og = None) (fields : expression S.t)  =
  pexp_record (List.map (fun (name, exp) -> (lident name, exp)) (S.bindings fields)) og


let new_name name = Fmt.str "%a" Ident.pp (Ident.create name ~loc)

(*map helpers*)
let map_name n = let prefix = if (n <= 3) then "Gen." else "" in
  if (n <= 0) then raise (Failure "<= 0 in map_name");
  if (n = 1) then prefix^"map" else prefix^"map" ^ (Int.to_string n)
let rec collect_by_threes l = match l with
  | x::y::z::l' -> [x; y; z]::(collect_by_threes l')
  | [] -> []
  | _ -> [l]


let mk_map_body fn args : expression =
  let fn_args : string list = mk_arg_names (List.length args) "arg" in
  let g_body : expression = mk_app (evar fn) (List.map evar fn_args) in
  let g_args : string list list = collect_by_threes fn_args in
  let g : expression = mk_fn g_args g_body in
  let args_body = collect_by_threes args in
  let args_body : (string option * string list) list = List.map (fun l -> match List.length l with
      | 1 -> (None, l)
      | 2 -> (Some (map_name 2), "tuple"::l)
      | 3 -> (Some (map_name 3), "triple":: l)
      | _ -> raise (Failure "incorrect collect by 3s")
    ) args_body in
  let args_body : expression list = List.map (fun (fn_opt, args) ->
      match fn_opt with | Some fn ->  mk_app (evar fn) (List.map evar args)
                        | None -> assert (List.length args = 1); evar (List.hd args)
    ) args_body in
  let body : expression =
    let small_map : expression = evar (map_name (List.length g_args)) in
    mk_app small_map ((evar "g")::args_body) in
  [%expr let [%p pvar "g"] = [%e g] in [%e body] ]


let mk_map n rem : expression =
  let name : string = map_name n in
  let rhs : expression = if (n <= 3) then [%expr [%e evar name]] else
      let (arg_names : string list) = (mk_arg_names n "gen") in
      let fn_arg = "f" in
      let body : expression = mk_map_body fn_arg arg_names in
      mk_fn_single (fn_arg::arg_names) body
  in
  [%expr let [%p pvar name] = [%e rhs] in [%e rem]]

let add_tuple b body = if b then
    let tuple = mk_fn_single ["a"; "b"] [%expr (a, b)] in
    [%expr let tuple = [%e tuple] in [%e body]] else body

let add_triple b body = if b then
    let triple = mk_fn_single ["a"; "b"; "c"] [%expr (a, b, c)] in
    [%expr let triple = [%e triple] in [%e body]] else body


(* ask jan this fn is actually well defined right?
   if (List.length l <= 3) then [] else
   let threed = collect_by_threes l in
   if (List.length threed <= 3) then [] (*no additional maps needed*)
   else (List.length threed)::(maps_needed threed) *)

(*which maps need to be defined to make a mapn
if tuple needs to be defined, if triple needs to be defined*)
let rec maps_needed n acc : ISet.t * bool * bool =
  if (n <= 3) then acc else
    let threes = n / 3 in
    let toplevel_groups = threes + (if (n mod 3) = 0 then 0 else 1) in
    (* this many groups at the top. at most one extra group because if there are two
                           extra they can be combined with map2,
       **)
    let (acc, tuple, _) = maps_needed toplevel_groups acc in
    (ISet.add n acc, (n mod 3) = 2 || tuple, true)
    (*triple must be true since you've divided into 3s *)


(*the functions*)

let mk_cmd_pat (cmd : string) (cmd_ele: cmd_ele) = 
  ppat_construct (lident cmd) (match cmd_ele.args with
      | [] -> None
      | _::_ as args ->
        Some (ppat_tuple (List.map (fun arg -> pvar arg.name) args) ))


let mk_cmd_cases ?(state_name = None) (cmd : Ast3.cmd) (rhs : expression S.t)  =
  List.map (fun (cmd_cstr, cmd_ele) ->
      let pattern = mk_cmd_pat cmd_cstr cmd_ele in
      let rhs = S.find cmd_cstr rhs in
      let rhs = match state_name with
        | None -> rhs
        | Some state_name ->
          [%expr let [%p pvar cmd_ele.targ_name] = [%e evar state_name] in [%e rhs]] in
      case ~lhs:pattern ~guard:None ~rhs) 
    (S.bindings cmd)

(*rhs is cmd_name -> (that case of the match)*)
let mk_cmdres_cases ~state_name:state_name (cmd : Ast3.cmd) (rhs : expression S.t) =
  let mk_res_pat cmd_cstr (cmd_ele : Ast3.cmd_ele) : pattern =
    let (typ, rpat) = match cmd_ele.ret with
      (*start here change this with r_name in mk_postcond if it works*)
      | [] -> raise (Failure ("empty return list in" ^ cmd_cstr ))
      (*start here replace all empty return lists in earlier phase*)
      | [ret] -> pvar (typ_to_str ~args:true ~capitalize:true ret.typ), pvar ret.name
      | _ :: _ -> raise (Failure ("tuple return type in " ^ cmd_cstr ^ " not yet supported")) in
    let typ = if cmd_ele.pure then typ else [%pat? Result ([%p typ], Exn)] in
    [%pat? Res (([%p typ], _), [%p rpat])] in
  List.map (fun (cmd_cstr, (cmd_ele : cmd_ele)) ->
      let pattern = ppat_tuple [mk_cmd_pat cmd_cstr cmd_ele; mk_res_pat cmd_cstr cmd_ele] in
     let rhs = S.find cmd_cstr rhs in
     let rhs = [%expr let [%p pvar cmd_ele.targ_name] = [%e evar state_name] in [%e rhs]] in
     case ~lhs:pattern ~guard:None ~rhs)
    (S.bindings cmd)

let mk_cmd (cmd : Ast3.cmd) : structure_item =
  let mk_variant (cmd : Ast3.cmd) =
    Ptype_variant (List.map (fun (name, (cmd_ele : Ast3.cmd_ele)) ->
        constructor_declaration ~name:(noloc name)
          ~args:(Pcstr_tuple (List.map mk_core_typ cmd_ele.args))
          ~res: None)
        (S.bindings cmd)) in
  let pstr_type: rec_flag -> type_declaration list -> structure_item = pstr_type in
  (*ask jan about hardcoding this attribute *)
  let no_attributes = type_declaration ~name:(noloc "cmd") ~params:[] ~cstrs:[]
      ~kind:(mk_variant cmd) ~private_:Public ~manifest:None in
  pstr_type Recursive [{no_attributes with ptype_attributes = [show_attribute]}]


let mk_state (state: Ast3.state): structure_item  =
  let mk_record_typ state = Ptype_record (List.map
      (fun (name, typ) -> label_declaration ~name:(noloc name) ~mutable_:Immutable
~type_:(typ_to_core_type typ)) (S.bindings state)) in     
  let td = type_declaration ~name:(noloc "state") ~params:[] ~cstrs:[]
      ~kind:(mk_record_typ state) ~private_:Public ~manifest:None in
  pstr_type Recursive [td]

(* 1) first find out which maps are needed. an int Set.
   2) make the body assuming those maps are already defined (mk_arb_cmd_body)
   3) fold over 1 using mkmap to add the maps at the front of 2)

   1) move over the cmd. get the number of args for each constructor 

   2) move over the arb command.
   constructor -> (mapname: string, fun: function which applies the constructor its arguments
                                    (first argument to mapname),
                                    list of the generators which are arguments to mapname)

*)
(*start here make it more likely to feed the state into a command when the types match*)
let mk_arb_cmd (cmd: Ast3.cmd) (arb_cmd: Ast3.arb_cmd) =
  let (maps_needed, tuple, triple) =
    S.fold (fun _ gens acc -> maps_needed (List.length gens) acc) arb_cmd (ISet.empty, false, false)
  in
  let mk_arb_cmd_body (cmd : Ast3.cmd) arb_cmd =
    (*for_oneof is args to Gen.oneof
      constructor -> Some (map, fn, generators) if there are args for constructor
                     None otherwise*)
    let for_oneof : (expression * expression * expression list) option S.t =
      S.mapi (fun cmd_constr gens ->
          if (List.length gens) = 0 then None else 
            let map = evar (map_name (List.length gens)) in
            let args : string list = (List.map (fun x -> x.name) (S.find cmd_constr cmd).args) in
            let constr_args : expression = pexp_tuple (List.map evar args) 
            in
            let fn : expression =  mk_fn_single args (pexp_construct (lident cmd_constr)
                                                        (Some constr_args)
                                                     ) in
            Some (map, fn, gens)
        ) arb_cmd in
    let for_oneof :expression list = List.map (fun (cmd, gen) -> match gen with
        |  None -> [%expr return [%e evar cmd]]
        | Some (map, fn, gens) ->
          pexp_apply map ((Nolabel, fn)::(List.map (fun e -> (Nolabel, e)) gens)))
        (S.bindings for_oneof) in
    [%expr QCheck.make ~print:show_cmd Gen.(oneof [%e elist for_oneof ])] in
  let body : expression = ISet.fold mk_map maps_needed (mk_arb_cmd_body cmd arb_cmd)
  (*add all the maps on top of the body*)
                        |> add_tuple tuple (*add tuple definition*)
                        |> add_triple triple (*add triple definition*)

  in
  [%stri let arb_cmd _s = [%e body]]


(*start here can't use the variable s as could be bound for the arguments
ortac does something about this where it doesnt let you reuse variable names for args
globally*)
  (*start here make it support old and some of the fields not being listed,
  only the ones which are modified are listed*)
let mk_next_state (cmd: Ast3.cmd) (next_state : Ast3.next_state) (state : state) ~state_name:state_name
  ~cmd_name:cmd_name =
  let state_var = evar state_name in
  let cmd_var = evar cmd_name in
  let rhs : expression S.t =
    S.map (fun (nsc : next_state_case) ->
        (*if all the fields are set then no need to use original*)
        (*start there need to check that cardinal of state is not 0*)
        let og = if S.cardinal nsc.next = S.cardinal state then None else (Some state_var) in
        let record = if S.cardinal nsc.next = 0 then state_var else mk_record ~og nsc.next in
        match nsc.pres with
          [] -> record (*no preconditions so automatically move to next state*)
        | _::_ -> [%expr if [%e conjoin nsc.pres] then
            [%e record]
          else [%e state_var]
        ]
      )
      next_state in
  let body = pexp_match cmd_var (mk_cmd_cases ~state_name:(Some state_name) cmd rhs) in
  [%stri let next_state [%p pvar cmd_name] [%p pvar state_name] = [%e body]]



let mk_precond (cmd: Ast3.cmd) (precond : Ast3.precond)
~state_name:state_name
    ~cmd_name:cmd_name =
 (* let patterns : pattern list = List.map (fun cmd cmd_ele ->
    )
      (S.bindings cmd) *)
  let precond : expression S.t = S.map conjoin precond in 
    let body = pexp_match (evar cmd_name) (mk_cmd_cases ~state_name:(Some state_name)
                                             cmd precond) in
  [%stri let precond [%p pvar cmd_name] [%p pvar state_name] = [%e body]]


let mk_run m_name (cmd : Ast3.cmd) (run: Ast3.run) ~cmd_name:cmd_name ~sut_name:sut_name =
  let res : expression S.t = S.mapi (fun cmd_cstr (ret, pure) ->
      let typ = match ret with
      | [] -> [%expr unit]
      | [ret] -> evar (typ_to_str ~args:true ret.typ)
      | _ :: _ -> raise (Failure ("tuple return type in " ^ cmd_cstr ^ " not yet supported")) in
      let typ = if pure then typ else [%expr result [%e typ] exn] in
      let cmd = S.find cmd_cstr cmd in
      let args = (evar sut_name)::(List.map (fun arg -> evar arg.name) cmd.args) in
      let fn = (evar (m_name ^ "." ^ (String.uncapitalize_ascii cmd_cstr))) in 
      let app = if pure
        then mk_app fn args
        else let (args, rem) = splitn (List.length args - 1) args in 
          mk_app (evar "protect") ((mk_app fn args)::rem) in
      [%expr Res ([%e typ], [%e app])]
    ) run in
  let body = pexp_match (evar cmd_name) (mk_cmd_cases cmd res) in
  [%stri let run [%p pvar cmd_name] [%p pvar sut_name] = [%e body]]


let mk_init_state (init_state: Ast3.init_state) =
  [%stri let init_state = [%e mk_record init_state]]

let mk_postcond cmd (postcond: Ast3.postcond ) ~cmd_name:cmd_name ~state_name:state_name =
  let rhs : expression S.t = S.mapi (fun cmd_cstr cmd_ele ->
      let pc_case = S.find cmd_cstr postcond in
      let r_name : string  = match cmd_ele.ret with
        | [] -> raise (Failure ("empty return list in" ^ cmd_cstr ))
        | [ret] -> ret.name
        | _ :: _ -> raise (Failure ("tuple return type in " ^ cmd_cstr ^ " not yet supported")) in

      let checks_conj = conjoin pc_case.checks in (*if all the checks then body
                                               else r = Error Invalid_argument *)
      let ensures_conj = conjoin pc_case.ensures  in
      let true_branch = if cmd_ele.pure then ensures_conj else
          let exn_name = new_name "exn" in
          let exn_match = pexp_match (evar ex_name) pc_case.raises in
          (*will it fall through here if an unexpected exception is raised start here*)
          let result_cases =
            [%expr match [%e evar r_name] with
| Error [%pat? pvar exn_name] -> [%e exn_match] 
          | Ok [%pat? pvar r_name] -> [%e ensures_conj] ] in 
      let false_branch = [%expr match [%e r_name] with
        Error (Invalid_argument _) -> true
        | _ -> false
      ] in
[%expr if [%e checks_conj] then [%e true_branch] else [%e false_branch]]

     (*leftover code? *)
      let ensures_conj = if pure then ensures_conj else
          [%expr match [%e evar r_name] with
            | Ok [%p pvar r_name] -> ensures_conj
            | Error exn -> Fmt.pf stderr "unexpected exception raised in %s\n%!" cmd_cstr;
              raise exn ] (*start here ask jan if this is good error handling*) in 
      let body = List.fold_right (fun raises body ->
          [%expr if [%e raises.condition] then r = Error [%e raises.exn]]
        ) pc.raises ensures_conj

     (*if raises condition then r = Error (that raises)*)
    ) cmd
let res_name = new_name "res" in
  let body = pexp_match (pexp_tuple [(evar cmd_name); (evar res_name)]) (mk_cmdres_cases cmd rhs) in
  [%stri let postcond [%p pvar cmd_name] [%p pvar state_name] [%p pvar res_name] = [%e body]]


let structure runtime (stm : Ast3.stm) : Parsetree.structure_item list =
  (*  Drv.print_ udriver ; *)
  let incl : Parsetree.structure_item =
    (pmod_ident (lident stm.module_name) |> include_infos |> pstr_include) in
  (*include statement for the user module*)
  let second : Parsetree.structure_item = pstr_module
      (module_binding
         ~name:{ txt = Some "Ortac_runtime"; loc }
         ~expr:(pmod_ident (lident runtime))) (*module Ortac_runtime = Ortac_runtime
                                              do i need this?*)
  in
  let open1 = open_infos ~expr:(pmod_ident (lident "QCheck")) ~override:Fresh |> pstr_open in
  let open2 = open_infos ~expr:(pmod_ident (lident "STM")) ~override:Fresh |> pstr_open in
  let sut = pstr_type Recursive [type_declaration ~name:(noloc "sut") ~params:[] ~cstrs:[]
                                   ~kind:Ptype_abstract ~private_:Public
                                   ~manifest:(Some (ptyp_constr (lident (stm.module_name ^ ".t")) []))] in
  let state = mk_state stm.state in
  let cmd = mk_cmd stm.cmd in
  let cmd_name = Fmt.str "%a" Ident.pp (Ident.create "c" ~loc) in
  let state_name = Fmt.str "%a" Ident.pp (Ident.create "s" ~loc) in
  let sut_name = Fmt.str "%a" Ident.pp (Ident.create "sut" ~loc) in
  let init_sut = [%stri let init_sut = [%e evar (stm.module_name ^ ".init_sut")]] in
  let  cleanup = [%stri let cleanup _ = () ] in 
  let arb_cmd = mk_arb_cmd stm.cmd stm.arb_cmd in
  let next_state = mk_next_state stm.cmd stm.next_state stm.state
      ~state_name ~cmd_name in
  let run = mk_run stm.module_name stm.cmd stm.run ~cmd_name ~sut_name in
  let init_state = mk_init_state stm.init_state in
  let precond = mk_precond stm.cmd stm.precond
      ~state_name ~cmd_name in
  [incl;second; open1; open2; sut ; state; cmd; init_sut; cleanup; arb_cmd;
next_state; run; init_state; precond]

