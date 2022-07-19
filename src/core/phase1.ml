(*go from a TAST into a Dv.driver *)
module W = Warnings
  open Types

open Ppxlib
open Builder
(* open Sexplib.Std *)
open Gospel
module T = Translation
module Ident = Identifier.Ident
module F = Failure
  module Ts = Drv_stm.Translated_stm


let term_printer  (t : Tterm.term)  =
  Fmt.str "%a" Tterm_printer.print_term t

(*start here fix this look at translate.ml wtih_invariants*)
let with_invariants ~driver ~term_printer (self, invariants) (type_ : Ts.type_)  =
  let silly _one _two _three _four = () in 
    let _ = (silly driver term_printer self) invariants in
  { type_ with invariants = [] }


let type_ ~(driver : Drv_stm.t) ~ghost (td : Tast.type_declaration) : Drv_stm.t =
  let name = td.td_ts.ts_ident.id_str in
  let loc = td.td_loc in
  let mutable_ = Mutability.type_declaration ~driver td in
  let type_ = Ts.type_ ~name ~loc  ~mutable_ ~ghost in
  (*line above sets all models and invariants to empty*)
  let process ~(type_ : Ts.type_) (spec : Tast.type_spec) =
    let term_printer = Fmt.str "%a" Tterm_printer.print_term in
    (*shows up only in the invariant function
    how is it allowed to use mutability . max??*)
    let mutable_ = Mutability.(max (type_.Ts.mutable_) (type_spec ~driver spec)) in
    (*mutability is the maximum of the mutability gotten from the driver and the mutability
      in the spec*)
    let models = type_.Ts.models in
    let (type_ : Ts.type_) =
      type_
      (*add back in the names of the models but nothing else*)
      |> with_invariants ~driver ~term_printer spec.ty_invariants
      (*need to support invariants later, start here*)
    in
    { type_ with mutable_ ; models }
  in
  let type_ = Option.fold ~none:type_ ~some:(process ~type_) td.td_spec in
  let type_item : Ts.structure_item = Ts.Type type_ in
   driver |> Drv_stm.add_translation type_item |> Drv_stm.add_type td.td_ts type_
(*type declarations get added to both translation and type lists*)

let types ~driver ~ghost =
  List.fold_left (fun driver -> type_ ~driver ~ghost) driver

(*converts from a TAST term back to a ppx expression.
some ortac specific stuff which i am taking out*)
let unsafe_term ~driver (t : Tterm.term) : expression =
  let unsupported m = raise (W.Error (W.Unsupported m, loc)) in
  match t.t_node with
   Tquant (Tterm.(Tforall | Texists),  _,  _ ) -> unsupported "ill formed quantification"
  | _ -> Translation.unsafe_term ~driver:driver t


let bool_term ~driver (fail : string) t =
  try
    Ok
      [%expr
        try [%e unsafe_term ~driver t] (*try and make this expression*)
        with e -> (*if it raises an exception
                    *)
          raise (Failure [%e estring fail])] (*estring makes the string into an expression
                                             just like evar makes the string into a variable*)
     with W.Error t -> Error t 
 
let with_checks ~driver (checks: Tterm.term list) (value : Translated.value): Translated.value =
  let checks =
    List.map
      (fun t ->
          let txt = term_printer t in
         let loc = t.Tterm.t_loc in 
         let term = bool_term ~driver "checks" t in
         let translations =
           Result.map 
              (fun exp -> (exp, exp)
              ) (* because you dont need two checks for
                does raise and doesnt raise invalid_arg
                                       just get the original check content,
            should change the check type in Translated i guess
                   start here
                *)
             term 
         in
         { txt; loc; Translated.translations } )
      checks
  in
  { value with checks }

let with_pre ~driver ~term_printer pres (value : Translated.value) =
  let preconditions = List.map (fun t ->
      let txt = term_printer t in
      let loc = t.Tterm.t_loc in
      let translation = bool_term ~driver "pre or post" t in 
      ({ txt; loc; translation } : Translated.term)) pres
  in
  { value with preconditions }

let with_post ~driver ~term_printer pres (value : Translated.value) =
  let postconditions = List.map (fun t ->
      let txt = term_printer t in
      let loc = t.Tterm.t_loc in
      let translation = bool_term ~driver "pre or post" t in 
      ({ txt; loc; translation } : Translated.term)) pres
  in
  { value with postconditions }


let value ~driver ~ghost (vd : Tast.val_description) =
  let name = vd.vd_name.id_str in
  let loc = vd.vd_loc in
  let register_name = "hoho register name" in
  let arguments = List.map (Translate.var_of_arg ~driver:driver) vd.vd_args in
  (*extracts name, label, and type of the argument. sets modified and consumed to false. *)
  let returns = List.map (Translate.var_of_arg ~driver:driver) vd.vd_ret in
  let pure = false in
  let value =
    Translated.value ~name ~loc ~register_name ~arguments ~returns ~pure ~ghost
      (*sets checks, preconditions, postconditions, xpostconditions to empty*)
  in
  let process ~value (spec : Tast.val_spec) =
  (*  print_endline("sp_text is");
      print_endline(spec.sp_text); *)
    let term_printer = term_printer  in
    let value =
      value
      |> with_checks ~driver spec.sp_checks 
      |> with_pre ~driver ~term_printer spec.sp_pre
      |> with_post ~driver ~term_printer spec.sp_post
    (*  |> with_xposts ~driver ~term_printer spec.sp_xpost
        (*throw all of these out for now start here*)
      |> with_consumes spec.sp_cs
        |> with_modified spec.sp_wr *)
    in
    { value with pure = spec.sp_pure }
  in
  let value = Option.fold ~none:value ~some:(process ~value) vd.vd_spec in
  (*process the spec if it exists*)
  let value_item = Translated.Value value in
  let driver =
    if value.pure then
      let ls = Drv.get_ls driver [ name ] in
      Drv.add_function ls name driver
      (*only pure functions get added to the driver function list ...*)
    else driver
  in
  (* Translated.print_term (List.hd value.preconditions); *)
  Drv.add_translation value_item driver

(*starts with empty driver (from ortac_core.signature)*)
let signature ~driver s : Drv_stm.t =
  (* Printf.printf "\ntast is:\n%s%!" (s |> Tast.sexp_of_signature |> string_of_sexp
                                     ); *)
  List.fold_left
    (fun driver (sig_item : Tast.signature_item) ->
       match sig_item.sig_desc with
       | Sig_val (vd, ghost) when vd.vd_args <> [] -> value ~driver ~ghost vd
       | Sig_val (_, _) -> driver (*ignoring constants*) 
       | Sig_type (_rec, td, ghost) -> types ~driver:driver ~ghost td
       (* | Sig_function func when Option.is_none func.fun_ls.ls_value ->
          predicate ~driver func*)
     (*  | Sig_function func -> function_ ~driver func
         still idk what goes in here
     *)
       (*  | Sig_axiom ax -> axiom ~driver ax *)
       | _ -> driver)
    driver s
