val generate : string -> out_channel -> unit
(** [generate path output] generates the code of the tests corresponding to the
    specifications present in [path] in the monolith configuration and prints it
    on the [output] channel *)
