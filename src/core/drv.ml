open Sexplib.Std
open Gospel.Tmodule

module L = Map.Make (Gospel.Symbols.LS)
module T = Map.Make (Gospel.Ttypes.Ts)



type sil = Translated.structure_item list
[@@deriving sexp_of]


(*you throw out everything except the module name,
  the translations,
  the types
  this is very wasteful.
*)

type t = {
  module_name : string; (*Example for example.mli
                        dont need this.*)
  stdlib : string L.t; (*map from symbol to string. dont need this. *)
  env : namespace; (*... all the reserved keywords? and stdlibrary stuff?
                   dont need this*)
  translations : Translated.structure_item list;
  types : Translated.type_ T.t; (*includes all the types in scope
                                  including the sut
                                  tysymbol to type_
                                  *)
  functions : string L.t; (*empty for example, not sure*)
}
let print_types = T.iter
    (fun tsymbol -> fun t ->
       print_endline("\ntsymbol:");
       Core.Sexp.output_hum stdout (Gospel.Ttypes.sexp_of_tysymbol tsymbol);
     print_endline("\ntype:");
       Core.Sexp.output_hum stdout (Translated.sexp_of_type_ t))

let print_ls = L.iter 
    (fun symbol -> fun s -> 
       print_endline("\nfsymbol:");
       Core.Sexp.output_hum stdout (Gospel.Symbols.sexp_of_lsymbol symbol);
Printf.printf "\nfn:%s\n%!" s)

let print_t t = (Printf.printf "module name: %s\n%!" t.module_name);
    (Gospel.Tmodule.print_ns "print_ns 1st arg" Fmt.stdout t.env);
  Printf.printf "translations:\n%!";
  Core.Sexp.output_hum stdout (sexp_of_sil t.translations);
  Printf.printf "\nend of translations.\ntypes:%!";
  print_types t.types;
  print_ls t.functions



let get_env get ns path =
  try get ns path
  with Not_found ->
    Fmt.(
      failwith "Internal error: path `%a' was not found"
        (list ~sep:(any ".") string)
        path)

let get_ls_env = get_env ns_find_ls
let get_ts_env = get_env ns_find_ts
let translate_stdlib ls t = L.find_opt ls t.stdlib
let add_translation i t = { t with translations = i :: t.translations }
let add_type ts i t = { t with types = T.add ts i t.types }
let get_type ts t = T.find_opt ts t.types
let add_function ls i t = { t with functions = L.add ls i t.functions }
let find_function ls t = L.find ls t.functions
let is_function ls t = L.mem ls t.functions
let get_ls t = get_ls_env t.env
let get_ts t = get_env ns_find_ts t.env

let stdlib_types =
  let open Translated in
  let loc = Ppxlib.Location.none in
  let ghost = Gospel.Tast.Nonghost in
  [
    ([ "unit" ], type_ ~name:"unit" ~loc
       ~mutable_:Immutable ~ghost);
    ([ "string" ], type_ ~name:"string" ~loc ~mutable_:Immutable ~ghost);
    ([ "char" ], type_ ~name:"char" ~loc ~mutable_:Immutable ~ghost);
    ([ "float" ], type_ ~name:"float" ~loc ~mutable_:Immutable ~ghost);
    ([ "bool" ], type_ ~name:"bool" ~loc ~mutable_:Immutable ~ghost);
    ([ "integer" ], type_ ~name:"integer" ~loc ~mutable_:Immutable ~ghost);
    ( [ "option" ],
      type_ ~name:"option" ~loc
        ~mutable_:(Dependant (function [ m ] -> m | _ -> assert false))
        ~ghost );
    ( [ "list" ],
      type_ ~name:"list" ~loc
        ~mutable_:(Dependant (function [ m ] -> m | _ -> assert false))
        ~ghost );
    ( [ "Gospelstdlib"; "seq" ],
      type_ ~name:"seq" ~loc
        ~mutable_:(Dependant (function [ m ] -> m | _ -> assert false))
        ~ghost );
    ( [ "Gospelstdlib"; "bag" ],
      type_ ~name:"bag" ~loc
        ~mutable_:(Dependant (function [ m ] -> m | _ -> assert false))
        ~ghost );
    ( [ "Gospelstdlib"; "ref" ],
      type_ ~name:"ref" ~loc ~mutable_:(Dependant (fun _ -> Mutable)) ~ghost );
    ( [ "array" ],
      type_ ~name:"array" ~loc ~mutable_:(Dependant (fun _ -> Mutable)) ~ghost
    );
    ( [ "Gospelstdlib"; "set" ],
      type_ ~name:"set" ~loc
        ~mutable_:(Dependant (function [ m ] -> m | _ -> assert false))
        ~ghost );
    ([ "int" ], type_ ~name:"int" ~loc ~mutable_:Immutable ~ghost);
  ]

let stdlib =
  [
    ([ "None" ], "None");
    ([ "Some" ], "Some");
    ([ "[]" ], "[]");
    ([ "infix ::" ], "(::)");
    ([ "infix =" ], "(=)");
    ([ "prefix !" ], "!");
    ([ "Gospelstdlib"; "infix +" ], "Ortac_runtime.Z.add");
    ([ "Gospelstdlib"; "infix -" ], "Ortac_runtime.Z.sub");
    ([ "Gospelstdlib"; "infix *" ], "Ortac_runtime.Z.mul");
    ([ "Gospelstdlib"; "infix /" ], "Ortac_runtime.Z.div");
    ([ "Gospelstdlib"; "mod" ], "Ortac_runtime.Z.rem");
    ([ "Gospelstdlib"; "pow" ], "Ortac_runtime.Z.pow");
    ([ "Gospelstdlib"; "logand" ], "Ortac_runtime.Z.logand");
    ([ "Gospelstdlib"; "prefix -" ], "Ortac_runtime.Z.neg");
    ([ "Gospelstdlib"; "infix >" ], "Ortac_runtime.Z.gt");
    ([ "Gospelstdlib"; "infix >=" ], "Ortac_runtime.Z.geq");
    ([ "Gospelstdlib"; "infix <" ], "Ortac_runtime.Z.lt");
    ([ "Gospelstdlib"; "infix <=" ], "Ortac_runtime.Z.leq");
    ([ "Gospelstdlib"; "integer_of_int" ], "Ortac_runtime.Z.of_int");
    ([ "Gospelstdlib"; "abs" ], "Ortac_runtime.Z.abs");
    ([ "Gospelstdlib"; "min" ], "Ortac_runtime.Z.min");
    ([ "Gospelstdlib"; "max" ], "Ortac_runtime.Z.max");
    ([ "Gospelstdlib"; "succ" ], "Ortac_runtime.Z.succ");
    ([ "Gospelstdlib"; "pred" ], "Ortac_runtime.Z.pred");
    ([ "Gospelstdlib"; "Array"; "make" ], "Ortac_runtime.Array.make");
    ([ "Gospelstdlib"; "Array"; "length" ], "Ortac_runtime.Array.length");
    ([ "Gospelstdlib"; "Array"; "get" ], "Ortac_runtime.Array.get");
    ([ "Gospelstdlib"; "Array"; "for_all" ], "Ortac_runtime.Array.for_all");
  ]

let stdlib_stm =
  [
    ([ "None" ], "None");
    ([ "Some" ], "Some");
    ([ "[]" ], "[]");
    ([ "infix ::" ], "(::)");
    ([ "infix =" ], "(=)");
    ([ "prefix !" ], "!");
    ([ "Gospelstdlib"; "infix +" ], "Z.add");
    ([ "Gospelstdlib"; "infix -" ], "Z.sub");
    ([ "Gospelstdlib"; "infix *" ], "Z.mul");
    ([ "Gospelstdlib"; "infix /" ], "Z.div");
    ([ "Gospelstdlib"; "mod" ], "Z.rem");
    ([ "Gospelstdlib"; "pow" ], "Z.pow");
    ([ "Gospelstdlib"; "logand" ], "Z.logand");
    ([ "Gospelstdlib"; "prefix -" ], "Z.neg");
    ([ "Gospelstdlib"; "infix >" ], "Z.gt");
    ([ "Gospelstdlib"; "infix >=" ], "Z.geq");
    ([ "Gospelstdlib"; "infix <" ], "Z.lt");
    ([ "Gospelstdlib"; "infix <=" ], "Z.leq");
    ([ "Gospelstdlib"; "integer_of_int" ], "Z.of_int");
    ([ "Gospelstdlib"; "abs" ], "Z.abs");
    ([ "Gospelstdlib"; "min" ], "Z.min");
    ([ "Gospelstdlib"; "max" ], "Z.max");
    ([ "Gospelstdlib"; "succ" ], "Z.succ");
    ([ "Gospelstdlib"; "pred" ], "Z.pred");
    ([ "Gospelstdlib"; "Array"; "make" ], "Ortac_runtime.Array.make");
    ([ "Gospelstdlib"; "Array"; "length" ], "Ortac_runtime.Array.length");
    ([ "Gospelstdlib"; "Array"; "get" ], "Ortac_runtime.Array.get");
    ([ "Gospelstdlib"; "Array"; "for_all" ], "Ortac_runtime.Array.for_all");
  ]

(*maybe driver is just to keep track of what is supported?*)
let init module_name env =
  let stdlib =
    List.fold_left
      (fun acc (path, ocaml) ->
        let ls = get_ls_env env path in
        L.add ls ocaml acc)
      L.empty stdlib
  in
  let types =
    List.fold_left
      (fun acc (path, type_) ->
        let ls = get_ts_env env path in
        T.add ls type_ acc)
      T.empty stdlib_types
  in
  { module_name; stdlib; env; translations = []; types; functions = L.empty }

let init_stm module_name env =
  let stdlib =
    List.fold_left
      (fun acc (path, ocaml) ->
         let ls = get_ls_env env path in
         L.add ls ocaml acc)
      L.empty stdlib in
  {module_name; stdlib = stdlib; env; translations = []; types = T.empty;
   functions = L.empty }

let map_translation ~f t = List.rev_map f t.translations
let iter_translation ~f t = List.iter f (List.rev t.translations)
let module_name t = t.module_name

