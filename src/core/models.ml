module Ortac_models = struct
  type  model_type = string * bool [@@deriving sexp]
end

module STM_models = struct
  type model_type = (string * Gospel.Ttypes.ty) list [@@deriving sexp]
end

(*maybe its better to just change the representation because no one
is even using that right now*)
