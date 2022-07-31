let generate path output =
  let module_name = Ortac_core.Utils.module_name_of_path path in
  let output = Format.formatter_of_out_channel output in
  Gospel.Parser_frontend.parse_ocaml_gospel path
  |> Ortac_core.Utils.type_check [] path
  |> fun (env, sigs) ->
  assert (List.length env = 1);
 (* print_endline("typed signature items are");
    Core.Sexp.output_hum stdout (Gospel.Tast.sexp_of_signature sigs); *)
    Ortac_core.Ortac.signature ~runtime:"Ortac_runtime" ~module_name (List.hd env)
    sigs
  |> Fmt.pf output "%a@." Ppxlib_ast.Pprintast.structure

(*this is how you can print a ppx ast*)


    (* Ppxlib_ast.Pprintast.expression **)
