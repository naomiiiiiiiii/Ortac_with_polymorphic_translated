let signature ~runtime ~module_name namespace s =
  let driver = Drv.init module_name namespace in
  let translated = Translate.signature ~driver s in
  Report.emit_warnings Fmt.stderr translated;
  (*ok so I guess here all the warnings are caught, this is what they do with the warnings*)
  Generate.structure runtime translated
