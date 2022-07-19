include Ex3
module Ortac_runtime = Ortac_runtime
let type_invar __error___003_ __position___004_ __self___002_ =
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
            pos_fname = "ex3.mli";
            pos_lnum = 8;
            pos_bol = 114;
            pos_cnum = 114
          };
        Ortac_runtime.stop =
          {
            pos_fname = "ex3.mli";
            pos_lnum = 13;
            pos_bol = 315;
            pos_cnum = 344
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
               { term = "10\n  "; term_kind = Pre; exn = e })
              |> (Ortac_runtime.Errors.register __error__005_);
            true))
  then
    (Ortac_runtime.Violated_invariant { term = "10\n  "; position = Pre }) |>
      (Ortac_runtime.Errors.register __error__005_);
  Ortac_runtime.Errors.report __error__005_;
  (let (t, silly2) =
     try create silly c
     with
     | Silly as __e___007_ ->
         (((match __e___007_ with
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
                        { term = "\n    ensur"; term_kind = Check; exn = e })
                       |> (Ortac_runtime.Errors.register __error__005_);
                     true))
           then
             (Ortac_runtime.Uncaught_checks { term = "\n    ensur" }) |>
               (Ortac_runtime.Errors.register __error__005_);
           Ortac_runtime.Errors.report __error__005_);
          raise __e___007_)
     | Invalid_argument _ as e ->
         ((if
             (try
                Ortac_runtime.Z.geq (Ortac_runtime.Z.of_int silly)
                  (Ortac_runtime.Z.of_int 5)
              with
              | e ->
                  ((Ortac_runtime.Specification_failure
                      { term = "\n    ensur"; term_kind = Check; exn = e })
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
                        { term = "\n    ensur"; term_kind = Check; exn = e })
                       |> (Ortac_runtime.Errors.register __error__005_);
                     true))
           then
             (Ortac_runtime.Uncaught_checks { term = "\n    ensur" }) |>
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
                        { term = "\n    ensur"; term_kind = Check; exn = e })
                       |> (Ortac_runtime.Errors.register __error__005_);
                     true))
           then
             (Ortac_runtime.Uncaught_checks { term = "\n    ensur" }) |>
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
                { term = "-> silly = "; term_kind = Post; exn = e })
               |> (Ortac_runtime.Errors.register __error__005_);
             true))
   then
     (Ortac_runtime.Violated_invariant
        { term = "-> silly = "; position = Post })
       |> (Ortac_runtime.Errors.register __error__005_);
   if
     not
       (try
          Ortac_runtime.Z.geq (Ortac_runtime.Z.of_int silly)
            (Ortac_runtime.Z.of_int 5)
        with
        | e ->
            ((Ortac_runtime.Specification_failure
                { term = "\n    ensur"; term_kind = Check; exn = e })
               |> (Ortac_runtime.Errors.register __error__005_);
             true))
   then
     (Ortac_runtime.Uncaught_checks { term = "\n    ensur" }) |>
       (Ortac_runtime.Errors.register __error__005_);
   type_invar __error__005_ Post t;
   Ortac_runtime.Errors.report __error__005_;
   (t, silly2))
let nonsense t1 t2 =
  let __error__006_ =
    Ortac_runtime.Errors.create
      {
        Ortac_runtime.start =
          {
            pos_fname = "ex3.mli";
            pos_lnum = 15;
            pos_bol = 346;
            pos_cnum = 346
          };
        Ortac_runtime.stop =
          {
            pos_fname = "ex3.mli";
            pos_lnum = 17;
            pos_bol = 462;
            pos_cnum = 464
          }
      } "nonsense" in
  type_invar __error__006_ Pre t2;
  type_invar __error__006_ Pre t1;
  Ortac_runtime.Errors.report __error__006_;
  (let (t3, t4) =
     try nonsense t1 t2
     with
     | Stack_overflow | Out_of_memory as e ->
         ((type_invar __error__006_ XPost t2;
           type_invar __error__006_ XPost t1;
           Ortac_runtime.Errors.report __error__006_);
          raise e)
     | e ->
         ((Ortac_runtime.Unexpected_exception { allowed_exn = []; exn = e })
            |> (Ortac_runtime.Errors.register __error__006_);
          (type_invar __error__006_ XPost t2;
           type_invar __error__006_ XPost t1;
           Ortac_runtime.Errors.report __error__006_);
          raise e) in
   type_invar __error__006_ Post t2;
   type_invar __error__006_ Post t1;
   type_invar __error__006_ Post t4;
   type_invar __error__006_ Post t3;
   Ortac_runtime.Errors.report __error__006_;
   (t3, t4))
