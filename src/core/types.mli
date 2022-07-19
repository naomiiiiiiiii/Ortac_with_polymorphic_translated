module Mutability (M : Translated.MODEL): sig
  open Translated
  open Drv 
  module Translated : (module type of (Sig_item (M)))
  module  Drv : (module type of  (Drv (M)))
  val max : Translated.mutability -> Translated.mutability -> Translated.mutability
  val ty : driver:Drv.t -> Gospel.Ttypes.ty -> Translated.mutability

  val type_declaration :
    driver:Drv.t -> Gospel.Tast.type_declaration -> Translated.mutability

  val type_spec : driver:Drv.t -> Gospel.Tast.type_spec -> Translated.mutability
end

