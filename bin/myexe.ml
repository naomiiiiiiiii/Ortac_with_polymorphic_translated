open Ortac_core


module S = Map.Make (String)

let map_of_list l = List.fold_right (fun (k, v) acc -> S.add k v acc) l S.empty 

let signature ~_runtime ~module_name namespace s =
  let driver = Drv.init module_name namespace in
  let (translated: Drv.t) = Phase1.signature ~driver s in
let translations = translated.Drv.translations in
  let out =  Phase2.init_state translations
      (map_of_list [("field1", Ast3.List Ast3.Integer);
                                          ("field2", Ast3.String)])
                                          in
   S.mapi (fun name exp -> Printf.printf "field name: %s\nfield exp: %s\n%!"
                name (Ppxlib_ast.Pprintast.string_of_expression exp)) out

let test path output =
  let module_name = Ortac_core.Utils.module_name_of_path path in
  let _output = Format.formatter_of_out_channel output in
  Gospel.Parser_frontend.parse_ocaml_gospel path
  |> Ortac_core.Utils.type_check [] path
  |> fun (env, sigs) ->
  assert (List.length env = 1);
    signature ~_runtime:"Ortac_runtime" ~module_name (List.hd env)
    sigs


(* let _ = Cli.main Cli.Default path None () <-- so annoying
that this doesn't work, ask jan*)

let main () =
   let inpath = "example.mli" in
(*let outpath = stdout in*)
 (* let channel = open_out outpath in*)
let channel = stdout in test inpath channel

  (*(try Ortac_default.generate inpath channel
  with Gospel.Warnings.Error e ->
    Fmt.epr "%a@." Gospel.Warnings.pp e;
    exit 1) *)

let _  = main ()
