include Ortac_ex
module Ortac_runtime = Ortac_runtime
let __invariant___001_ __error___003_ __position___004_ __self___002_ =
  if
    not
      (try
         Ortac_runtime.Z.lt (Ortac_runtime.Z.of_int 1)
           (Ortac_runtime.Z.of_int 0)
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               {
                 term = "(1:integer < 0:integer):prop";
                 term_kind = __position___004_;
                 exn = e
               })
              |> (Ortac_runtime.Errors.register __error___003_);
            true))
  then
    (Ortac_runtime.Violated_invariant
       { term = "(1:integer < 0:integer):prop"; position = __position___004_
       })
      |> (Ortac_runtime.Errors.register __error___003_)
let create silly c =
  let __error__005_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "../src/default/ortac_ex.mli";
            pos_lnum = 6;
            pos_bol = 152;
            pos_cnum = 152
          };
        Ortac_runtime.stop =
          {
            pos_fname = "../src/default/ortac_ex.mli";
            pos_lnum = 13;
            pos_bol = 482;
            pos_cnum = 511
          }
      } "create" in
  if
    not
      (try
         Ortac_runtime.Z.gt (Ortac_runtime.Z.of_int c)
           (Ortac_runtime.Z.of_int 0)
       with
       | e ->
           ((Ortac_runtime.Specification_failure
               { term = " sill"; term_kind = Pre; exn = e })
              |> (Ortac_runtime.Errors.register __error__005_);
            true))
  then
    (Ortac_runtime.Violated_invariant { term = " sill"; position = Pre }) |>
      (Ortac_runtime.Errors.register __error__005_);
  Ortac_runtime.Errors.report __error__005_;
  (let (t, silly2) =
     try create silly c
     with
     | Silly as __e___006_ ->
         (((match __e___006_ with
            | Silly ->
                if
                  not
                    (try
                       (Ortac_runtime.Z.of_int silly) =
                         (Ortac_runtime.Z.of_int 6)
                     with
                     | e ->
                         ((Ortac_runtime.Specification_failure
                             {
                               term =
                                 "((integer_of_int \nsilly:int):integer = 6:integer):prop";
                               term_kind = XPost;
                               exn = e
                             })
                            |> (Ortac_runtime.Errors.register __error__005_);
                          true))
                then
                  (Ortac_runtime.Violated_invariant
                     {
                       term =
                         "((integer_of_int \nsilly:int):integer = 6:integer):prop";
                       position = XPost
                     })
                    |> (Ortac_runtime.Errors.register __error__005_)
            | _ -> assert false);
           if
             not
               (try
                  Ortac_runtime.Z.geq (Ortac_runtime.Z.of_int silly)
                    (Ortac_runtime.Z.of_int 5)
                with
                | e ->
                    ((Ortac_runtime.Specification_failure
                        { term = "ouou"; term_kind = Check; exn = e })
                       |> (Ortac_runtime.Errors.register __error__005_);
                     true))
           then
             (Ortac_runtime.Uncaught_checks { term = "ouou" }) |>
               (Ortac_runtime.Errors.register __error__005_);
           Ortac_runtime.Errors.report __error__005_);
          raise __e___006_)
     | Invalid_argument _ as e ->
         ((if
             (try
                Ortac_runtime.Z.geq (Ortac_runtime.Z.of_int silly)
                  (Ortac_runtime.Z.of_int 5)
              with
              | e ->
                  ((Ortac_runtime.Specification_failure
                      { term = "ouou"; term_kind = Check; exn = e })
                     |> (Ortac_runtime.Errors.register __error__005_);
                   true))
           then
             (Ortac_runtime.Unexpected_checks { terms = [] }) |>
               (Ortac_runtime.Errors.register __error__005_);
           Ortac_runtime.Errors.report __error__005_);
          raise e)
     | Stack_overflow | Out_of_memory as e ->
         ((if
             not
               (try
                  Ortac_runtime.Z.geq (Ortac_runtime.Z.of_int silly)
                    (Ortac_runtime.Z.of_int 5)
                with
                | e ->
                    ((Ortac_runtime.Specification_failure
                        { term = "ouou"; term_kind = Check; exn = e })
                       |> (Ortac_runtime.Errors.register __error__005_);
                     true))
           then
             (Ortac_runtime.Uncaught_checks { term = "ouou" }) |>
               (Ortac_runtime.Errors.register __error__005_);
           Ortac_runtime.Errors.report __error__005_);
          raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__005_);
          (if
             not
               (try
                  Ortac_runtime.Z.geq (Ortac_runtime.Z.of_int silly)
                    (Ortac_runtime.Z.of_int 5)
                with
                | e ->
                    ((Ortac_runtime.Specification_failure
                        { term = "ouou"; term_kind = Check; exn = e })
                       |> (Ortac_runtime.Errors.register __error__005_);
                     true))
           then
             (Ortac_runtime.Uncaught_checks { term = "ouou" }) |>
               (Ortac_runtime.Errors.register __error__005_);
           Ortac_runtime.Errors.report __error__005_);
          raise e) in
   if
     not
       (try
          Ortac_runtime.Z.lt (Ortac_runtime.Z.of_int silly2)
            (Ortac_runtime.Z.of_int 10)
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                {
                  term =
                    "((integer_of_int \nsilly2:int):integer < 10:integer):prop";
                  term_kind = Post;
                  exn = e
                })
               |> (Ortac_runtime.Errors.register __error__005_);
             true))
   then
     (Ortac_runtime.Violated_invariant
        {
          term = "((integer_of_int \nsilly2:int):integer < 10:integer):prop";
          position = Post
        })
       |> (Ortac_runtime.Errors.register __error__005_);
   if
     not
       (try
          Ortac_runtime.Z.geq (Ortac_runtime.Z.of_int silly)
            (Ortac_runtime.Z.of_int 5)
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "ouou"; term_kind = Check; exn = e })
               |> (Ortac_runtime.Errors.register __error__005_);
             true))
   then
     (Ortac_runtime.Uncaught_checks { term = "ouou" }) |>
       (Ortac_runtime.Errors.register __error__005_);
   __invariant___001_ __error__005_ Post t;
   Ortac_runtime.Errors.report __error__005_;
   (t, silly2))
