module W = Warnings
open Ppxlib
open Builder
open Ast3
module F = Failure
module T = Translation
module S = Map.Make (String)
module ISet = Set.Make (Int)




let top_typ_to_str t = match t with
  | Integer -> "Ortac_runtime.Z.t"
  | Int -> "int"
  | _ -> "start here"

let lident_dot s =
  let names = String.split_on_char '.' s in
  assert (List.length names >= 1);
  List.fold_left (fun acc name ->
match acc with
  None -> Some (Lident name)
  | Some li -> Some (Ldot (li, name))) None names |> Option.get |> noloc

let rec typ_to_core_type (t : Ast3.typ) : core_type =
  ptyp_constr (t |> top_typ_to_str |> lident_dot)  (List.map typ_to_core_type (Ast3.get_typ_args t))


(*how to get a core type out of a string?*)
let mk_core_typ (arg : Ast3.ocaml_var) : core_type = typ_to_core_type arg.typ
  (* [%type int][@subst let t : string = (type_to_string t)] *)
  (* [%type [%t typ_to_string arg.typ]] *)


let fake_loc : Location.t =
  let fake_pos : Lexing.position = {pos_fname = "fake"; pos_lnum = (-1); pos_bol = (-1); pos_cnum = (-1) } in
  {loc_start = fake_pos; loc_end = fake_pos; loc_ghost = true }

let mk_cmd (cmd : Ast3.cmd) : structure_item =
  let mk_variant (cmd : Ast3.cmd) =
    Ptype_variant (List.map (fun (name, (cmd_ele : Ast3.cmd_ele)) ->
        constructor_declaration ~name:(noloc (String.capitalize_ascii name))
          ~args:(Pcstr_tuple (List.map mk_core_typ cmd_ele.args))
          ~res: None)
      (S.bindings cmd)) in
  let pstr_type: rec_flag -> type_declaration list -> structure_item = pstr_type in
  (*ask jan about hardcoding this attribute *)
  let show_attribute : attribute = {attr_name = (noloc "deriving");
                         attr_payload =
                           PStr
                             [{pstr_desc =
                                 Pstr_eval
                                   ({pexp_desc =
                                       Pexp_apply
                                         ({pexp_desc = Pexp_ident {txt = Lident "show"; loc = fake_loc};
                                           pexp_loc_stack = []; pexp_attributes = []; pexp_loc = fake_loc},
                                          [(Nolabel,
                                            {pexp_desc =
                                               Pexp_record
                                                 ([({txt = Lident "with_path"; loc = fake_loc},
                                                    {pexp_desc =
                                                       Pexp_construct ({txt = Lident "false"; loc = fake_loc}, None);
                                                     pexp_loc_stack = []; pexp_attributes = []; pexp_loc = fake_loc})],
                                                  None);
                                             pexp_loc_stack = []; pexp_attributes = []; pexp_loc = fake_loc})]);
                                     pexp_loc_stack = []; pexp_attributes = []; pexp_loc = fake_loc},
                                    []);
                               pstr_loc = fake_loc;
                              }];
                                    attr_loc = fake_loc
                                   }  in
  let no_attributes = type_declaration ~name:(noloc "cmd") ~params:[] ~cstrs:[]
       ~kind:(mk_variant cmd) ~private_:Public ~manifest:None in
     pstr_type Recursive [{no_attributes with ptype_attributes = [show_attribute]}]




(*need to generate the maps you need first
then just write mapn *)
(*Set -> (int: expression) have 
Set -> [map, (fun i -> Set i), int ] want 
*)
(*start here make it more likely to feed the state into a command when the types match*)


let map5 f gen1 gen2 gen3 gen4 gen5 =
let g (one, two, three) four five = f one two three four five in
map3 g (map3 triple gen1 gen2 gen3) gen4 gen5
let test = ppat_tuple

(*
let econcat e1 e2 = [%expr [%e e1] [%e e2]]
let eflatten (l : expression list): expression = List.fold_right (fun e acc -> econcat e acc) l
   (evar " ") *)

let map_name n = "map" ^ (Int.to_string n)

ppat_tuple

let mk_fn (arg_names : string list list) (body: expression) : expression =
  List.fold_right (fun arg_tuple acc ->
      pexp_fun Nolabel None (ppat_tuple (List.map pvar arg_tuple)) acc) arg_names body

let mk_fn_single args =
  mk_fn (List.map (fun x -> [x]) args)

let mk_arg_names n prefix = List.init n (fun i -> prefix^ (Int.to_string i))

let rec collect_by_threes l = match l with
  | x::y::z::l' -> [x; y; z]::(collect_by_threes l')
  | [] -> []
  | _ -> [l]

let mk_app (fn : expression) (args : expression list) = pexp_apply fn
    (List.map (fun arg -> (Nolabel, arg)) args)


let mk_map_body fn args : expression =
  let g_args : string list = mk_arg_names (List.length args) "arg" in
  let g_args_collect : string list list = collect_by_threes g_args in
  let g_body : expression = mk_app (evar fn) (List.map evar g_args) in
  let g : expression = mk_fn g_args_collect g_body in
  let args_collect = collect_by_threes g_args in
  let args_body : (string option * string list) list = List.map (fun l -> match List.length l with
      | 1 -> (None, l)
      | 2 -> (Some (map_name 2), "double"::l)
      | 3 -> (Some (map_name 3), "triple":: l)
      | _ -> raise (Failure "incorrect collect by 3s")
    ) args_collect in
  let args_body : expression list = (List.map (fun (fn_opt, args) ->
      match fn_opt with | Some fn ->  mk_app (evar fn) (List.map evar args)
                        | None -> assert (List.length args = 1); evar (List.hd args)
    ) args_body) in
  let body : expression = let small_map : expression = evar (map_name (List.length g_args)) in
    mk_app small_map ((evar "g")::args_body) in
  [%expr let [%pat [%p pstring "g"]] = [%e g] in [%e body] ]

let mkmap n rem : expression =
  let name : string = map_name n in
  let rhs : expression = if (n < 4) then [%expr [%e evar name]] else
      let (arg_names : string list) = (mk_arg_names n "gen") in
      let fn_arg = "f" in
      let body : expression = mk_map_body fn_arg arg_names in
      mk_fn_single (fn_arg::arg_names) body
  in
  [%expr let [%pat [%p pvar name]] = [%e rhs] in [%e rem]]



(* 1) first find out which maps are needed. an int list.
   2) make the body using those maps
   3) fold over 1 using mkmap to add the maps at the front

   1) move over the cmd. get the number of args. 

   2) move over the arb command.
   Cmd_constr -> (mapname: string, fun: expression which uses
   the constr and the arguments of cmd, expression list taken straight out of the arb command)

*)

(*how many map fns are needed to make a map that takes n generators*)

unit unit one one


let rec maps_needed i : int list =
  if (i <= 9) then  [] else
    let threes = i / 3 in (* this many units. at most one extra group because if there are two
                           extra they can be combined with map3 **)
    let toplevel_groups = threes + (if (i mod 3) = 0 then 0 else 1) in
    toplevel_groups::(maps_needed toplevel_groups)


  (* ask jan this fn is actually well defined right?
  if (List.length l <= 3) then [] else
    let threed = collect_by_threes l in
    if (List.length threed <= 3) then [] (*no additional maps needed*)
    else (List.length threed)::(maps_needed threed) *)

(*      9/3 is 3. fine 
 map3 g (map3 triple one two three) (map3 triple four five six) (map3 triple seven eight nine)


 map4 g (map3 triple one two three) (map3 triple four five six)
(map3 triple seven eight nine) ten

        in which_maps_help (ISet.of_list [1;2;3]) *)

let mk_arb_cmd (cmd: Ast3.cmd) (arb_cmd: Ast3.arb_cmd) =












let structure runtime (stm : Ast3.stm) : Parsetree.structure_item list =
  (*  Drv.print_t driver ; *)
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
  let cmd = mk_cmd stm.cmd in
  (*  let arb_cmd = mk_arb_cmd stm.arb_cmd in *)
 [incl;second; open1; open2; cmd]
