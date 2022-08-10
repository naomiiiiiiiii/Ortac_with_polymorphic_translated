module W = Warnings
open Ppxlib
open Gospel
open Fmt
open Builder
open Translated
module F = Failure
module Ident = Identifier.Ident

let rec pattern pn =
  let pattern' (p : Tterm.pattern) = pattern p.p_node in
  match pn with
  | Tterm.Pwild -> ppat_any
  | Tterm.Pvar v -> pvar (str "%a" Ident.pp v.vs_name)
  | Tterm.Papp (l, pl) when Symbols.is_fs_tuple l ->
      ppat_tuple (List.map pattern' pl)
  | Tterm.Papp (l, pl) ->
      let args =
        if pl = [] then None else Some (ppat_tuple (List.map pattern' pl))
      in
      ppat_construct (lident l.ls_name.id_str) args
  | Tterm.Por (p1, p2) -> ppat_or (pattern' p1) (pattern' p2)
  | Tterm.Pas (p, v) ->
      ppat_alias (pattern' p) (noloc (str "%a" Ident.pp v.vs_name))
  | Tterm.Pinterval (c1, c2) -> ppat_interval (Pconst_char c1) (Pconst_char c2)
  | Tterm.Pconst c -> ppat_constant c

type bound = Inf of expression | Sup of expression

(*bounds is only necessary for translating gospel quantifiers*)
let rec bounds ~driver ~loc (var : Symbols.vsymbol) (t1 : Tterm.term)
    (t2 : Tterm.term) =
  let unsupported () =
    raise (W.Error (Unsupported "ill formed quantification", loc))
  in
  (* [comb] extracts a bound from an the operator [f] and expression [e].
     [right] indicates if [e] is on the right side of the operator. *)
  let comb ~right (f : Symbols.lsymbol) e =
    match f.ls_name.id_str with
    | "infix >=" -> if right then Inf e else Sup e
    | "infix <=" -> if right then Sup e else Inf e
    | "infix <" -> if right then Sup (epred e) else Inf (esucc e)
    | "infix >" -> if right then Inf (esucc e) else Sup (epred e)
    | _ -> unsupported ()
  in
  let bound = function
    | Tterm.Tapp (f, [ { t_node = Tvar vs; _ }; t ])
      when vs.vs_name = var.vs_name ->
        comb ~right:true f (unsafe_term ~driver t)
    | Tterm.Tapp (f, [ t; { t_node = Tvar vs; _ } ])
      when vs.vs_name = var.vs_name ->
        comb ~right:false f (unsafe_term ~driver t)
    | _ -> unsupported ()
  in
  match (bound t1.t_node, bound t2.t_node) with
  | Inf start, Sup stop | Sup stop, Inf start -> (start, stop)
  | _ -> unsupported ()

(*converts from a TAST term back to a ppx expression.
some ortac specific stuff*)
and unsafe_term ~driver (t : Tterm.term) : expression =
  let term = unsafe_term ~driver in
  let loc = t.t_loc in
  let unsupported m = raise (W.Error (W.Unsupported m, loc)) in
  match t.t_node with
  | Tvar { vs_name; _ } -> evar (str "%a" Ident.pp vs_name)
                         (*makes an expression out of the variable name*)
  | Tconst c -> econst c 
  | Tfield (t, f) -> pexp_field (term t) (lident f.ls_name.id_str)
  | Tapp (fs, []) when Symbols.(ls_equal fs fs_bool_true) -> [%expr true]
  (*multiple ways of parsing true*)
  | Tapp (fs, []) when Symbols.(ls_equal fs fs_bool_false) -> [%expr false]
  | Tapp (fs, tlist) when Symbols.is_fs_tuple fs ->
      List.map term tlist |> pexp_tuple (*makes a tuple, not sure
                                        how tuples larger than unit are resprented in fs*)
  | Tapp (ls, tlist) when Drv.is_function ls driver ->
  let f = Drv.find_function ls driver in
      eapply (evar f) (List.map term tlist)
  (*two ways of function application which both go to eapply f args.
  one where Drv.is_function ls driver and one where ls is fs_apply.
  what does this mean?*)
  | Tapp (ls, tlist) when Symbols.(ls_equal ls fs_apply) ->
      let f, args =
        match tlist with
        | [] -> assert false
        | x :: xs -> (term x, List.map term xs)
      in
      eapply f args
  | Tapp (ls, tlist) -> (
      Drv.translate_stdlib ls driver |> function
      | Some f -> eapply (evar f) (List.map term tlist)
                    (*if its in the standard library then translate it*)
      | None ->
          let func = ls.ls_name.id_str in
          if ls.ls_constr then
            (*true if the function is a construct?
            constructor?*)
            (if tlist = [] then None
            else Some (List.map term tlist |> pexp_tuple))
            |> pexp_construct (lident func)
            (*make a constructor application out of it*)
          else kstr unsupported "function application `%s`" func)
    (*^gospel standard library function is unsupported 
    here is where you would change things to make it so that lists,
    arrays, etc are supported in standard library*)
  | Tif (i, t, e) -> [%expr if [%e term i] then [%e term t] else [%e term e]]
  | Tlet (x, t1, t2) ->
      let x = str "%a" Ident.pp x.vs_name in
      [%expr
        let [%p pvar x] = [%e term t1] in
        [%e term t2]] (*everything in the expression parens inclduing
                      the keywords is part of the epxression*)
  | Tcase (t, ptl) ->
      List.map
        (fun (p, g, t) ->
          case ~guard:(Option.map term g) ~lhs:(pattern p.Tterm.p_node)
            ~rhs:(term t))
        ptl
      |> pexp_match (term t) (*just converts from a typed case back to a ppx casese*)
  | Tquant (Tterm.Tlambda, args, t) ->(*lambdas*)
      let t = term t in
      let args =
        List.map
          (fun (vs : Symbols.vsymbol) ->
            (Nolabel, pvar (str "%a" Ident.pp vs.vs_name)))
          args
      in
      efun args t 
 | Tquant (*forall and eints*)
      ( (Tterm.(Tforall | Texists) as quant),
        [ var ],
        Tterm.
          {
            t_node =
              Tbinop
                ( ((Timplies | Tand | Tand_asym) as op),
                  { t_node = Tbinop (Tand, t1, t2); _ },
                  p );
            _;
          } ) ->
      (match (quant, op) with
      | Tforall, Timplies | Texists, (Tand | Tand_asym) -> ()
      | _, _ -> unsupported "ill formed quantification");
      let start, stop = bounds ~driver ~loc var t1 t2 in
      let p = term p in
      let quant =
        evar
          (if quant = Tforall then "Ortac_runtime.Z.forall"
          else "Ortac_runtime.Z.exists")
      in
      let x = str "%a" Ident.pp var.vs_name in
      let func = pexp_fun Nolabel None (pvar x) p in
      eapply quant [ start; stop; func ] 
  | Tquant (_, _, _) -> unsupported "quantification"
  | Tbinop (op, t1, t2) -> (
      match op with
      | Tterm.Tand ->
          let vt1 = gen_symbol ~prefix:"__t1" () in
          let vt2 = gen_symbol ~prefix:"__t2" () in
          [%expr
            let [%p pvar vt1] = [%e term t1] in
            (*[%p for patter]
            pvar to turn vt1 into a PATTERN variable
            evar is for EXPRESSION variable*)
            let [%p pvar vt2] = [%e term t2] in
            [%e evar vt1] && [%e evar vt2]]
          (*&& is and asym.
            with Tand want them to both run for sure
            what is Tand in gospel?
          *)
      | Tterm.Tand_asym -> [%expr [%e term t1] && [%e term t2]]
      | Tterm.Tor ->
          let vt1 = gen_symbol ~prefix:"__t1" () in
          let vt2 = gen_symbol ~prefix:"__t2" () in
          [%expr
            let [%p pvar vt1] = [%e term t1] in
            let [%p pvar vt2] = [%e term t2] in
            [%e evar vt1] || [%e evar vt2]]
      | Tterm.Tor_asym -> [%expr [%e term t1] || [%e term t2]]
      | Tterm.Timplies -> [%expr (not [%e term t1]) || [%e term t2]]
      | Tterm.Tiff -> [%expr [%e term t1] = [%e term t2]])
  | Tnot t -> [%expr not [%e term t]]
  | Told _ -> unsupported "old operator"
  | Ttrue -> [%expr true]
  | Tfalse -> [%expr false]
(*converts the TAST term t into a ppx expression which is
(try [the expression inside the original term] with
e -> (fail) (evar "e") 
  )
  what the heck is e? it's literally just e. it means nothing.

  using nonexec and invariant_failure, the output of this ends up looking like
   (try
             Ortac_runtime.Z.gt (Ortac_runtime.Z.of_int box.capacity)
               (Ortac_runtime.Z.of_int 0)
           with
           | e ->
               ((Ortac_runtime.Specification_failure
                   {
                     term =
                       "((integer_of_int \n(box:'a t).capacity):integer > 0:integer):prop";
                     term_kind = __position___003_;
                     exn = e
                   })
                  |> (Ortac_runtime.Errors.register __error___002_);
                true))
*)
let term ~driver fail t =
  try
    Ok
      [%expr
        try [%e unsafe_term ~driver t] (*try and make this expression*)
        with e -> (*if it raises an exception
                    make an expression which raises fail instead*)
          [%e fail (evar "e")]; (*nonexec txt (evar "e")*)
          true] (*semicolon true because this expression has type boolean*)
  with W.Error t -> Error t

let conditions ~driver ~term_printer fail_violated fail_nonexec (pres : Tterm.term list) =
  List.map
    (fun t ->
      let txt = term_printer t in
      let loc = t.Tterm.t_loc in
      let translation =
        term ~driver (fail_nonexec txt) t
        |> Result.map (fun t ->
               [%expr if not [%e t] then [%e fail_violated txt]])
      in
      ({ txt; loc; translation } : term))
pres

let with_models ~driver:_ fields (type_ : type_) =
  let models =
    List.map (fun ((ls : Symbols.lsymbol), b) -> (ls.ls_name.id_str, b)) fields
  in
  { type_ with models }

let subst_invariant_fields var (t : Tterm.term) =
  let rec aux t =
    match t.Tterm.t_node with
    | Tapp (ls, []) when ls.ls_field ->
        { t with t_node = Tterm.Tfield (var, ls) }
    | Tvar _ | Tconst _ | Ttrue | Tfalse -> t
    | Tapp (ls, tl) ->
        let tl = List.map aux tl in
        let t_node = Tterm.Tapp (ls, tl) in
        { t with t_node }
    | Tfield (t, ls) ->
        let t = aux t in
        let t_node = Tterm.Tfield (t, ls) in
        { t with t_node }
    | Tif (t1, t2, t3) ->
        let t1 = aux t1 in
        let t2 = aux t2 in
        let t3 = aux t3 in
        let t_node = Tterm.Tif (t1, t2, t3) in
        { t with t_node }
    | Tterm.Tlet (vs, t1, t2) ->
        let t1 = aux t1 in
        let t2 = aux t2 in
        let t_node = Tterm.Tlet (vs, t1, t2) in
        { t with t_node }
    | Tterm.Tcase (t, ptl) ->
        let t = aux t in
        let ptl =
          List.map (fun (p, g, t) -> (p, Option.map aux g, aux t)) ptl
        in
        let t_node = Tterm.Tcase (t, ptl) in
        { t with t_node }
    | Tquant (q, vsl, t) ->
        let t = aux t in
        let t_node = Tterm.Tquant (q, vsl, t) in
        { t with t_node }
    | Tterm.Tbinop (op, t1, t2) ->
        let t1 = aux t1 in
        let t2 = aux t2 in
        let t_node = Tterm.Tbinop (op, t1, t2) in
        { t with t_node }
    | Tterm.Tnot t ->
        let t = aux t in
        let t_node = Tterm.Tnot t in
        { t with t_node }
    | Tterm.Told t ->
        let t = aux t in
        let t_node = Tterm.Told t in
        { t with t_node }
  in
  aux t

(* let invariants = List.map (invariant ~driver ~term_printer self) invariants in *)

let invariant ~driver ~term_printer self (invariant : Tterm.term) =
  let function_name = gen_symbol ~prefix:"__invariant_" () in
  let instance_id =
    match self with
    | None -> Ident.create ~loc (gen_symbol ~prefix:"__self_" ())
    | Some self -> self.Symbols.vs_name (*self, if it exists, becomes
                                        the name of the invariant?*)
  in
  let instance_arg = (Nolabel, pvar (Fmt.str "%a" Ident.pp instance_id)) in
  let instance_term =
    (* XXX This is not the correct type or location, but it doesn't matter for
       the translation *)
    Tterm_helper.t_var { vs_name = instance_id; vs_ty = Ttypes.ty_unit } loc
  in

  let register_name = gen_symbol ~prefix:"__error_" () in
  let register_name_arg = (Nolabel, pvar register_name) in
  let register_name = evar register_name in

  let position = gen_symbol ~prefix:"__position_" () in
  let eposition = evar position in
  let position_arg = (Nolabel, pvar position) in

  let violated term = F.violated_invariant eposition ~term ~register_name in
  let nonexec term exn =
    F.invariant_failure eposition ~term ~exn ~register_name
  in
  let txt = term_printer invariant in (*start here IS THIS THE GUILTY ONE*)
  let loc = invariant.Tterm.t_loc in
  let translation =
    let invariant = subst_invariant_fields instance_term invariant in
    term ~driver (nonexec txt) invariant
    |> Result.map (fun e -> [%expr if not [%e e] then [%e violated txt]])
    |> Result.map (efun [ register_name_arg; position_arg; instance_arg ])
    |> Result.map (fun e ->
           (function_name, [%stri let [%p pvar function_name] = [%e e]]))
  in
  { txt; loc; translation }

(*self, invariants: vsymbol option * term list*)
let with_invariants ~driver ~term_printer (self, invariants) (type_ : type_) =
  let invariants = List.map (invariant ~driver ~term_printer self) invariants in
  { type_ with invariants }

let with_consumes consumes (value : value) =
  let name (t : Tterm.term) =
    match t.t_node with
    | Tterm.Tvar vs -> Some (Fmt.str "%a" Tast.Ident.pp vs.vs_name)
    | _ -> None
  in
  let consumes = List.filter_map name consumes in
  let arguments =
    List.map (* not very efficient *)
      (fun (a : Translated.ocaml_var) ->
        if List.exists (fun c -> a.name = c) consumes then
          { a with consumed = true }
        else a)
      value.arguments
  in
  { value with arguments }

let with_modified modifies (value : value) =
  let name (t : Tterm.term) =
    match t.t_node with
    | Tterm.Tvar vs -> Some (Fmt.str "%a" Tast.Ident.pp vs.vs_name)
    | _ -> None
  in
  let modifies = List.filter_map name modifies in
  let arguments =
    List.map (* not very efficient *)
      (fun (a : Translated.ocaml_var) ->
        if List.exists (fun c -> a.name = c) modifies then
          { a with modified = true }
        else a)
      value.arguments
  in
  { value with arguments }

let with_pres ~driver ~term_printer pres (value : value) =
  let register_name = evar value.register_name in
  let violated term = F.violated `Pre ~term ~register_name in
  (*Pre is printed verbatim in 'position = Pre'
    gives you: Ortac_runtime.Violated_invariant { term = "ises "; position = Pre }
  *)
  let nonexec term exn = F.spec_failure `Pre ~term ~exn ~register_name in
  (*gives you ((Ortac_runtime.Specification_failure
               { term = "ises "; term_kind = Pre; exn = e })
              |> (Ortac_runtime.Errors.register __error__005_)*)
  (*in practice (evar "e") is always what is put in for extensionn*)
  let preconditions = conditions ~driver ~term_printer violated nonexec pres in
  { value with preconditions }

(*turns the term list of checks (eg c > 0)
into a big if for ortac
*)
let with_checks ~driver ~term_printer checks (value : value) =
  let register_name = evar value.register_name in
  (*evar converts from a string to a (variable) expression*)
  let nonexec (term: string) exn =
    F.spec_failure `Check ~term ~exn ~register_name in
  (*this is where the silly names get added into the spec failures
  using ppx*)
  let checks =
    List.map
      (fun t ->
        let txt = term_printer t in
        let loc = t.Tterm.t_loc in
        let term = term ~driver (nonexec txt) t in
        let translations =
          Result.map
            (fun t ->
              ( [%expr
                  if not [%e t] then
                    [%e F.uncaught_checks ~register_name ~term:txt]],
                [%expr if [%e t] then [%e F.unexpected_checks ~register_name]]
                (*how does this work if there are multiple checks, only one is violated,
                  invalid argument is raised? would there be an unexpected check?*)
              ))
            term
        in
        { txt; loc; translations })
      checks
  in
  { value with checks }

let with_posts ~driver ~term_printer posts (value : value) =
  let register_name = evar value.register_name in
  let violated term = F.violated `Post ~term ~register_name in
  let nonexec term exn = F.spec_failure `Post ~term ~exn ~register_name in
  let postconditions =
    conditions ~driver ~term_printer violated nonexec posts
  in
  { value with postconditions }

let with_constant_checks ~driver ~term_printer checks (constant : constant) =
  let register_name = evar constant.register_name in
  let violated term = F.violated `Pre ~term ~register_name in
  let nonexec term exn = F.spec_failure `Pre ~term ~exn ~register_name in
  let checks = conditions ~driver ~term_printer violated nonexec checks in
  { constant with checks }

(*matches on a pattern_node
the exception pattern in raises Silly pat -> term
goes from a Tast.pattern_node to a ppx pattern *)
let rec xpost_pattern ~driver exn = function
  | Tterm.Papp (ls, []) when Symbols.(ls_equal ls (fs_tuple 0)) -> pvar exn
  | Tterm.Papp (ls, _l) when not (Symbols.is_fs_tuple ls) -> assert false
  | Tterm.Por (p1, p2) ->
      ppat_or
        (xpost_pattern ~driver exn p1.p_node)
        (xpost_pattern ~driver exn p2.p_node)
  | Tterm.Pas (p, s) ->
      ppat_alias
        (xpost_pattern ~driver exn p.p_node)
        (noloc (str "%a" Tterm.Ident.pp s.vs_name))
  | pn -> ppat_construct (lident exn) (Some (pattern pn)) (*the names of the args get included in this pattern*)

let assert_false_case =
  case ~guard:None ~lhs:[%pat? _] ~rhs:[%expr assert false]

let with_xposts ~driver ~term_printer (xposts: (Ttypes.xsymbol *
                                                (Tterm.pattern * Tterm.term) list) list)
    (value : value) =
  (*the second element of xposts is the ptlist in xpost fn below*)
  print_endline("incoming xposts are:");
  Core.Sexp.output_hum Stdlib.stdout (Tast.sexp_of_xpost xposts); 
  let register_name = evar value.register_name in
  (*xpost processes one raises into a case list*)
  let xpost ((exn : Ttypes.xsymbol), (ptlist : (Tterm.pattern * Tterm.term) list)) =
    let name : string = exn.Ttypes.xs_ident.id_str in
    let cases =
      List.map
        (fun (p, t) ->
          let s = term_printer t in
          let nonexec exn = F.spec_failure `XPost ~term:s ~exn ~register_name in (*just the failure case for turning t into a term*)
          term ~driver nonexec t
          |> Result.map (fun (t : expression) -> (*turn the term into a case*)
                 case ~guard:None
                   ~lhs:(xpost_pattern ~driver name p.Tterm.p_node) (*make an xpost pattern*)
                   ~rhs:
                     [%expr
                       if not [%e t] then (*for you this should just be [%expr t] because you want
                                          postcond to simply return false*)
                         [%e F.violated `XPost ~term:s ~register_name]]))
        (* XXX ptlist must be rev because the cases are given in the
           reverse order by gospel <- this is false*)
        (List.rev ptlist)
    in
    if List.exists Result.is_error cases then
      List.filter_map (function Ok _ -> None | Error x -> Some x) cases
      |> Result.error
    else List.map Result.get_ok cases @ [ assert_false_case ] |> Result.ok (*case list is never empty without the assert_false_case
assert_false forces execution to halt in the middle of stm test, not what i want.
need to move that to default only *)
  in
  let xpostconditions = (*turn each tast xpost into a translated xpost*)
    List.map
      (fun xp ->
        let xs = fst xp in
        let exn = xs.Ttypes.xs_ident.id_str in
        let args =
          match xs.Ttypes.xs_type with
          | Ttypes.Exn_tuple l -> List.length l
          | Ttypes.Exn_record _ -> 1
        in
        let translation = xpost xp in
        { exn; args; translation }) (*keeps the number of args but not what they are
                                    but doesnt matter because what the args are is stored in the translation*)
      xposts
  in
  { value with xpostconditions }

let function_definition ~driver ls i t : term =
  let txt = Fmt.str "%a" Tterm_printer.print_term t in
  let loc = t.t_loc in
  let translation =
    let driver = Drv.add_function ls i driver in
    try Ok (unsafe_term ~driver t) with W.Error t -> Error t
  in
  { txt; loc; translation }

let axiom_definition ~driver ~register_name t : term =
  let register_name = evar register_name in
  let fail_violated = F.violated_axiom ~register_name in
  let fail_nonexec exn = F.axiom_failure ~exn ~register_name in
  let txt = Fmt.str "%a" Tterm_printer.print_term t in
  let loc = t.t_loc in
  let translation =
    term ~driver fail_nonexec t
    |> Result.map (fun check ->
           [%expr
             if not [%e check] then [%e fail_violated];
             [%e F.report ~register_name]])
  in
  { txt; loc; translation }
