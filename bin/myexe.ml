open Ortac_core


module S = Map.Make (String)


let signature ~runtime ~module_name namespace (s : Gospel.Tast.signature) =
  let driver = Drv.init module_name namespace in
  let (translated: Drv.t) = Phase1.signature ~driver s in (*switch must be in here
           let vsname (vs : Symbols.vsymbol) = Fmt.str "%a" Tast.Ident.pp vs.vs_name

                                                          *)
  Report.emit_warnings Fmt.stderr translated;
  (* print_endline("translations after phase 1 are:");
     Core.Sexp.output_hum stdout (Drv.sexp_of_sil translated.translations); *)
  let stm =  Phase2.stm translated in
  Phase3.structure runtime stm 
(*      (map_of_list [("field1", Ast3.List Ast3.Integer);
                                          ("field2", Ast3.String)])
                                          in
   S.mapi (fun name exp -> Printf.printf "field name: %s\nfield exp: %s\n%!"
                name (Ppxlib_ast.Pprintast.string_of_expression exp)) out *)

let generate path output =
  let module_name = Ortac_core.Utils.module_name_of_path path in
  let output = Format.formatter_of_out_channel output in
  Gospel.Parser_frontend.parse_ocaml_gospel path
  |> Ortac_core.Utils.type_check [] path
  |> fun (env, sigs) ->
  assert (List.length env = 1);
 (* print_endline("typed signature items are");
    Core.Sexp.output_hum stdout (Gospel.Tast.sexp_of_signature sigs); *)
    signature ~runtime:"Ortac_runtime" ~module_name (List.hd env)
      sigs
     |> Fmt.pf output "%a@." Ppxlib_ast.Pprintast.structure 



(* let _ = Cli.main Cli.Default path None () <-- so annoying
that this doesn't work, ask jan*)

let main () =
   let inpath = "example_exn.mli" in
   (* let outchannel = stdout in*)
   let channel = stdout in generate inpath channel 
(* 
  (try Ortac_default.generate inpath outchannel
  with Gospel.Warnings.Error e ->
    Fmt.epr "%a@." Gospel.Warnings.pp e;
    exit 1) *)

let _  = main ()
