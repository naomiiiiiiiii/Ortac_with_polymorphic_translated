(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Ppxlib
open Ttypes
open Tterm
open Symbols
open Sexplib.Std
module Ident = Identifier.Ident

(*labelled arg*)
type lb_arg =
  | Lunit  (** () *)
  | Lnone of vsymbol  (** x *)
  | Loptional of vsymbol  (** ?x *)
  | Lnamed of vsymbol  (** ~x *)
  | Lghost of vsymbol  (** \[x: t\] *)
[@@deriving show, sexp_of]

(* let _ = show_lb_arg Lunit *)

type xpost = (xsymbol * (pattern * term) list) list
[@@deriving show, sexp_of]
(*start here why is this a list list?*)


type val_spec = { 
  sp_args : lb_arg list;  (** Arguments *)
  sp_ret : lb_arg list;
      (** Return values. This is a list because of tuple destruction. *)
  sp_pre : term list;  (** Requires Preconditions *)
  sp_checks : term list;  (** Checks preconditions *)
  sp_post : term list;  (** Postconditions *)
  sp_xpost : xpost;
      (** Exceptional postconditions. *)
  sp_wr : term list;  (** Writes *)
  sp_cs : term list;  (** Consumes *)
  sp_diverge : bool;  (** Diverges *)
  sp_pure : bool;  (** Pure *)
  sp_equiv : string list;  (** Equivalent *)
  sp_text : string;
      (** String containing the original specificaion as written by the user *)
  sp_loc : (Location.t [@sexp.opaque]); (*global start loc*)
  [@printer fun fmt _ -> fprintf fmt "<Location.t>"]
      (** Specification location *)
}
[@@deriving show, sexp_of]

(*, @printer fun fmt _ -> fprintf fmt "<Location.t>"]*)

(*why is there a 'specification attribute' and vd_attrs
  but also a separate vd_spec*)
type val_description = {
  vd_name : Ident.t;
  vd_type : (core_type [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<core_type>"]
  vd_prim : string list;  (** primitive declaration *)
  vd_attrs : (attributes [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<attributes>"]
  vd_args : lb_arg list;
  vd_ret : lb_arg list;
  vd_spec : val_spec option;
  vd_loc : (Location.t [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<Location.t>"]
}
[@@deriving show, sexp_of]

type type_spec = {
  ty_ephemeral : bool;  (** Ephemeral *)
  ty_fields : (lsymbol * bool) list;
 (* Models (field symbol  * mutable)
the lsymbol has in it
      {name : string
      ls_args: the type of which it is a field? always a singleton list?
      ls_value: the type which it is? always Some?
      ls_field: amusingly false
      }
       *)
  ty_invariants : vsymbol option * term list;  (** Invariants *)
  ty_text : string;
      (** String containing the original specificaion as written by the user *)
  ty_loc : (Location.t [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<Location.t>"]
      (** Specification location *)
}
[@@deriving show, sexp_of]

type mutable_flag = Immutable | Mutable [@@deriving show, sexp_of]

(* used for both record declarations and variant declarations *)
type 'a label_declaration = {
  ld_field : 'a;
  ld_mut : mutable_flag;
  ld_loc : (Location.t [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<Location.t>"]
  ld_attrs : (attributes [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<attributes>"]
      (* l : T [@id1] [@id2] *)
}
[@@deriving show, sexp_of]

type rec_declaration = {
  rd_cs : lsymbol;
  rd_ldl : lsymbol label_declaration list;
}
[@@deriving show, sexp_of]

type constructor_decl = {
  cd_cs : lsymbol;
  (* constructor *)
  (* cd_ld is empty if defined through a tuple *)
  cd_ld : (Ident.t * ty) label_declaration list;
  cd_loc : (Location.t [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<Location.t>"]
  cd_attrs :(attributes [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<attributes>"]
      (* C of ... [@id1] [@id2] *)
}
[@@deriving show, sexp_of]

type type_kind =
  | Pty_abstract
  | Pty_variant of constructor_decl list  (** Invariant: non-empty list *)
  | Pty_record of rec_declaration
[@@deriving show, sexp_of]

type private_flag = Private | Public [@@deriving show, sexp_of]

type type_declaration = {
  td_ts : tysymbol;
  td_params : (tvsymbol * (variance * injectivity) [@sexp.opaque] ) list;
      [@printer
        fun fmt params ->
          fprintf fmt "%a"
            (Fmt.list ~sep:Fmt.comma (fun fmt (tv, _) ->
                 fprintf fmt "(%a, (<variance>, <injectivity>))" pp_tvsymbol tv))
            params]
  (* the core_type in uast can only be Ptyp_var _ or Ptyp_any
     according to the parser *)
      td_cstrs : (ty * ty * (Location.t [@sexp.opaque]) ) list;
      [@printer
        fun fmt cstrs ->
          fprintf fmt "%a"
            (Fmt.list ~sep:Fmt.comma (fun fmt (t0, t1, _) ->
                 fprintf fmt "(%a, %a, <Location.t>)" pp_ty t0 pp_ty t1))
            cstrs]
  td_kind : type_kind;
  td_private : private_flag;
  td_manifest : ty option;
  td_attrs :(attributes [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<attributes>"]
  td_spec : type_spec option;
  td_loc : (Location.t [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<Location.t>"]
}
[@@deriving show, sexp_of]

type axiom = {
  ax_name : Ident.t;  (** Name *)
  ax_term : term;  (** Definition *)
  ax_loc : (Location.t [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<Location.t>"]
      (** Location *)
  ax_text : string;
      (** String containing the original specificaion as written by the user *)
}
[@@deriving show, sexp_of]

type fun_spec = {
  fun_req : term list;  (** Preconditions *)
  fun_ens : term list;  (** Postconditions *)
  fun_variant : term list;  (** Variant *)
  fun_coer : bool;  (** Coercion *)
  fun_text : string;
      (** String containing the original specificaion as written by the user *)
  fun_loc : (Location.t [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<Location.t>"]
      (** Specification location *)
}
[@@deriving show, sexp_of]

type function_ = {
  fun_ls : lsymbol;  (** Function symbol *)
  fun_rec : bool;  (** Recursive *)
  fun_params : vsymbol list;  (** Arguments *)
  fun_def : term option;  (** Definition *)
  fun_spec : fun_spec option;  (** Specification *)
  fun_text : string;
      (** String containing the original specificaion as written by the user *)
  fun_loc : (Location.t [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<Location.t>"]
      (** Location *)
}
[@@deriving show, sexp_of]

type extension_constructor = {
  ext_ident : Ident.t;
  ext_xs : xsymbol;
  ext_kind : (extension_constructor_kind [@ sexp.opaque]);
      [@printer fun fmt _ -> fprintf fmt "<extension_constructor_kind>"]
  ext_loc : (Location.t [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<Location.t>"]
  ext_attributes :(attributes [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<attributes>"]
      (* C of ... [@id1] [@id2] *)
}
[@@deriving show, sexp_of]

type type_exception = {
  exn_constructor : extension_constructor;
  exn_loc : (Location.t [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<Location.t>"]
  exn_attributes :(attributes [@sexp.opaque]);
      (* ... [@@id1] [@@id2] *)
      [@printer fun fmt _ -> fprintf fmt "<attributes>"]
}
[@@deriving show, sexp_of]

type rec_flag = Nonrecursive | Recursive [@@deriving show, sexp_of]
type ghost = Nonghost | Ghost [@@deriving show, sexp]

type with_constraint =
  | Wty of Ident.t * type_declaration
  (* with type X.t = ...

     Note: the last component of the longIdent.t must match
     the name of the type_declaration. *)
  | Wmod of Ident.t * Ident.t
  (* with module X.Y = Z *)
  | Wtysubs of Ident.t * type_declaration
  (* with type X.t := ..., same format as [Pwith_type] *)
  | Wmodsubs of Ident.t * Ident.t
(* with module X.Y := Z *)
[@@deriving show, sexp_of]

type open_description = {
  opn_id : string list;
  opn_override : (Asttypes.override_flag [@ sexp.opaque] );
      [@printer fun fmt _ -> fprintf fmt "<Asttypes.override_flag>"]
  opn_loc : (Location.t [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<Location.t>"]
  opn_attrs :(attributes [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<attributes>"]
}
[@@deriving show, sexp_of]

type signature = signature_item list [@@deriving show, sexp_of]

and signature_item = {
  sig_desc : signature_item_desc;
  sig_loc : (Location.t [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<Location.t>"]
}
[@@deriving show, sexp_of]

and signature_item_desc =
  | Sig_val of val_description * ghost
  | Sig_type of rec_flag * type_declaration list * ghost
  (* type t1 = ... and ... and tn = ... *)
  | Sig_typext of (type_extension [@sexp.opaque] )
      [@printer fun fmt _ -> fprintf fmt "Sig_typext <type_extension>"]
  (* type t1 += ... *)
  | Sig_module of module_declaration
  (* module X : MT *)
  | Sig_recmodule of module_declaration list
  (* module rec X1 : MT1 and ... and Xn : MTn *)
  | Sig_modtype of module_type_declaration
  (* module type S = MT
     module type S *)
  (* these were not modified *)
  | Sig_exception of type_exception
  (* exception C of T *)
  | Sig_open of open_description * ghost
  (* open X *)
  | Sig_include of (include_description [@sexp.opaque] )
      [@printer fun fmt _ -> fprintf fmt "Sig_include <include_description>"]
  (* type t1 += ... *)
  (* include MT *)
  | Sig_class of (class_description list [@sexp.opaque] )
      [@printer fun fmt _ -> fprintf fmt "Sig_class <class_description list>"]
  (* type t1 += ... *)
  (* class c1 : ... and ... and cn : ... *)
  | Sig_class_type of (class_type_declaration list [@sexp.opaque] )
      [@printer
        fun fmt _ -> fprintf fmt "Sig_class_type <class_type_declaration list>"]
  (* type t1 += ... *)
  (* class type ct1 = ... and ... and ctn = ... *)
  | Sig_attribute of (attribute [@sexp.opaque] )
      [@printer fun fmt _ -> fprintf fmt "Sig_attribute <attribute>"]
  (* type t1 += ... *)
  (* [@@@id] *)
  | Sig_extension of (extension * attributes [@sexp.opaque] )
      [@printer
        fun fmt _ -> fprintf fmt "Sig_extension <extension * attributes>"]
  (* type t1 += ... *)
  (* [%%id] *)
  (* Specific to specification *)
  | Sig_use of string
  | Sig_function of function_
  | Sig_axiom of axiom
[@@deriving show, sexp_of]

and module_declaration = {
  md_name : Ident.t;
  md_type : module_type;
  md_attrs :(attributes [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<attributes>"]
  (* ... [@@id1] [@@id2] *)
  md_loc : (Location.t [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<Location.t>"]
}
[@@deriving show, sexp_of]

and module_type_declaration = {
  mtd_name : Ident.t;
  mtd_type : module_type option;
  mtd_attrs :(attributes [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<attributes>"]
  (* ... [@@id1] [@@id2] *)
  mtd_loc : (Location.t [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<Location.t>"]
}
[@@deriving show, sexp_of]

and module_type = {
  mt_desc : module_type_desc;
  mt_loc : (Location.t [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<Location.t>"]
  mt_attrs :(attributes [@sexp.opaque]); [@printer fun fmt _ -> fprintf fmt "<attributes>"]
      (* ... [@id1] [@id2] *)
}
[@@deriving show, sexp_of]

and module_type_desc =
  | Mod_ident of string list
  (* S *)
  | Mod_signature of signature
  (* sig ... end *)
  | Mod_functor of Ident.t * module_type option * module_type
  (* functor(X : MT1) -> MT2 *)
  | Mod_with of module_type * with_constraint list
  (* MT with ... *)
  | Mod_typeof of (module_expr [@sexp.opaque] )
      [@printer fun fmt _ -> fprintf fmt "Mod_typeof <module_expr>"]
  (* module type of ME *)
  | Mod_extension of (extension [@sexp.opaque] )
      [@printer fun fmt _ -> fprintf fmt "Mod_extension <extension>"]
  (* [%id] *)
  | Mod_alias of string list
(* (module M) *)
[@@deriving show, sexp_of]
