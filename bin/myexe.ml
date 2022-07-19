

(* let _ = Cli.main Cli.Default path None () <-- so annoying
that this doesn't work, ask jan*)

let main () =
   let inpath = "example.mli" in
(*let outpath = stdout in*)
 (* let channel = open_out outpath in*)
let channel = stdout in
  (try Ortac_default.generate inpath channel
  with Gospel.Warnings.Error e ->
    Fmt.epr "%a@." Gospel.Warnings.pp e;
    exit 1)

   let _ = main ()

