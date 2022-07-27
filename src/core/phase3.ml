module W = Warnings
open Ppxlib
open Builder
open Ast3
module F = Failure
module T = Translation
module S = Map.Make (String)

type test = Ortac_runtime.Z.t

let test : longident loc -> core_type list -> core_type = ptyp_constr



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

let mk_cmd (cmd : Ast3.cmd) : structure_item =
  let mk_variant (cmd : Ast3.cmd) =
    Ptype_variant (List.map (fun (name, (cmd_ele : Ast3.cmd_ele)) ->
        constructor_declaration ~name:(noloc (String.capitalize_ascii name))
          ~args:(Pcstr_tuple (List.map mk_core_typ cmd_ele.args))
          ~res: None)
      (S.bindings cmd)) in
  let pstr_type: rec_flag -> type_declaration list -> structure_item = pstr_type in
  pstr_type Recursive
    [type_declaration ~name:(noloc "cmd") ~params:[] ~cstrs:[]
       ~kind:(mk_variant cmd) ~private_:Public ~manifest:None 
    ]

let structure runtime (stm : Ast3.stm) : Parsetree.structure_item list =
  (*  Drv.print_t driver ; *)
  let first : Parsetree.structure_item =
    (pmod_ident (lident stm.module_name) |> include_infos |> pstr_include) in
  (*include statement for the user module*)
  let second : Parsetree.structure_item = pstr_module
      (module_binding
         ~name:{ txt = Some "Ortac_runtime"; loc }
         ~expr:(pmod_ident (lident runtime))) (*module Ortac_runtime = Ortac_runtime
                                              do i need this?*)
  in
  let cmd = mk_cmd stm.cmd   in
  first
  :: second
  :: [cmd]
