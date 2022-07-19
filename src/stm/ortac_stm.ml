let signature  ~module_name namespace s =
  let driver = Ortac_core.Drv.init module_name namespace in
  let translated = Ortac_core.Phase1.signature ~driver s in
  (* Ortac_core.Report.emit_warnings Fmt.stderr translated; *)
(*  Generate.structure runtime translated *)
translated

let stm_print = Ortac_core.Drv.print_t

let generate path _output =
  let module_name = Ortac_core.Utils.module_name_of_path path in
  let _output = Format.formatter_of_out_channel _output in
  Gospel.Parser_frontend.parse_ocaml_gospel path
  |> Ortac_core.Utils.type_check [] path
  |> fun (env, sigs) ->
  assert (List.length env = 1);
  signature  ~module_name (List.hd env)
    sigs
  |> stm_print
