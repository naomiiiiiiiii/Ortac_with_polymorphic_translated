include Atom
module Ortac_runtime = Ortac_runtime
open QCheck
type sut = Atom.t
type state = {
  contents: Ortac_runtime.Z.t }
type cmd =
  | Compare_and_set of int * int 
  | Exchange of int 
  | Five_args of int list * int list list * int * int * int 
  | Get 
  | Set of int [@@deriving show { with_path = false }]
let init_sut = Atom.init_sut
let cleanup _ = ()
let arb_cmd _s =
  let triple a b c = (a, b, c) in
  let tuple a b = (a, b) in
  let map5 f gen0 gen1 gen2 gen3 gen4 =
    let g (arg0, arg1, arg2) (arg3, arg4) = f arg0 arg1 arg2 arg3 arg4 in
    Gen.map2 g (Gen.map3 triple gen0 gen1 gen2) (Gen.map2 tuple gen3 gen4) in
  QCheck.make ~print:show_cmd
    (let open Gen in
       oneof
         [Gen.map2 (fun seen -> fun v_1 -> Compare_and_set (seen, v_1))
            (frequency [(1, small_nat); (20, int)])
            (frequency [(1, small_nat); (20, int)]);
         Gen.map (fun i -> Exchange i)
           (frequency [(1, small_nat); (20, int)]);
         map5
           (fun i_1 ->
              fun two ->
                fun three ->
                  fun four ->
                    fun five -> Five_args (i_1, two, three, four, five))
           (list (frequency [(1, small_nat); (20, int)]))
           (list (list (frequency [(1, small_nat); (20, int)])))
           (frequency [(1, small_nat); (20, int)])
           (frequency [(1, small_nat); (20, int)])
           (frequency [(1, small_nat); (20, int)]);
         return Get;
         Gen.map (fun s -> Set s) (frequency [(1, small_nat); (20, int)])])
let next_state c s_1 =
  match c with
  | Compare_and_set (seen, v_1) ->
      let atom_3 = s_1 in
      if
        (Ortac_runtime.Z.add (Ortac_runtime.Z.of_int seen)
           (Ortac_runtime.Z.of_int v_1))
          = (Ortac_runtime.Z.of_int 30)
      then { contents = (Ortac_runtime.Z.of_int v_1) }
      else s_1
  | Exchange i ->
      let atom_2 = s_1 in
      if
        Ortac_runtime.Z.gt (Ortac_runtime.Z.of_int i)
          (Ortac_runtime.Z.of_int 42)
      then { contents = (Ortac_runtime.Z.of_int i) }
      else s_1
  | Five_args (i_1, two, three, four, five) -> let atom_4 = s_1 in s_1
  | Get -> let atom = s_1 in s_1
  | Set s ->
      let atom_1 = s_1 in
      if
        (Ortac_runtime.Z.gt (Ortac_runtime.Z.of_int s)
           (Ortac_runtime.Z.of_int 0))
          &&
          ((Ortac_runtime.Z.of_int (get atom_1)) = (Ortac_runtime.Z.of_int 5))
      then { contents = (Ortac_runtime.Z.of_int s) }
      else s_1
let run c sut =
  match c with
  | Compare_and_set (seen, v_1) ->
      Res ((result bool exn), (protect (Atom.compare_and_set sut seen) v_1))
  | Exchange i -> Res ((result int exn), (protect (Atom.exchange sut) i))
  | Five_args (i_1, two, three, four, five) ->
      Res (bool, (Atom.five_args sut i_1 two three four five))
  | Get -> Res (int, (Atom.get sut))
  | Set s -> Res ((result unit exn), (protect (Atom.set sut) s))
let init_state = { contents = (Ortac_runtime.Z.of_int 0) }
let precond c (s_1 : state) =
  match c with
  | Compare_and_set (seen, v_1) ->
      let atom_3 = s_1 in
      (Ortac_runtime.Z.add (Ortac_runtime.Z.of_int seen)
         (Ortac_runtime.Z.of_int v_1))
        = (Ortac_runtime.Z.of_int 30)
  | Exchange i ->
      let atom_2 = s_1 in
      Ortac_runtime.Z.gt (Ortac_runtime.Z.of_int i)
        (Ortac_runtime.Z.of_int 42)
  | Five_args (i_1, two, three, four, five) -> let atom_4 = s_1 in true
  | Get -> let atom = s_1 in true
  | Set s ->
      let atom_1 = s_1 in
      (Ortac_runtime.Z.of_int (get atom_1)) = (Ortac_runtime.Z.of_int 5)
let postcond c s_1 res =
  match (c, res) with
  | (Compare_and_set (seen, v_1), Res ((Result (Bool, Exn), _), out_2)) ->
      let atom_3 = s_1 in
      if true
      then
        (match out_2 with
         | Error exn -> 
             (match exn with
              | Stack_overflow | Out_of_memory -> raise exn
              | _ -> false)
         | Ok out_2 -> true)
      else
        (match out_2 with | Error (Invalid_argument _) -> true | _ -> false)
  | (Exchange i, Res ((Result (Int, Exn), _), out_1)) ->
      let atom_2 = s_1 in
      if true
      then
        (match out_1 with
         | Error exn_1 ->
             (match exn_1 with
              | Stack_overflow | Out_of_memory -> raise exn_1
              | _ -> false)
         | Ok out_1 -> true)
      else 
        (match out_1 with | Error (Invalid_argument _) -> true | _ -> false)
  | (Five_args (i_1, two, three, four, five), Res ((Bool, _), out_3)) ->
      let atom_4 = s_1 in
      if true
      then true
      else
        (match out_3 with | Error (Invalid_argument _) -> true | _ -> false)
  | (Get, Res ((Int, _), v)) -> 
      let atom = s_1 in 
      if true
      then (Ortac_runtime.Z.of_int v) = atom.contents
      else (match v with | Error (Invalid_argument _) -> true | _ -> false)
  | (Set s, Res ((Result (Unit, Exn), _), _)) ->
      let atom_1 = s_1 in
      if
        Ortac_runtime.Z.gt (Ortac_runtime.Z.of_int s)
          (Ortac_runtime.Z.of_int 0)
      then
        (match ret_1 with
         | Error exn_2 ->
             (match exn_2 with
              | Stack_overflow | Out_of_memory -> raise exn_2
              | _ -> false)
         | Ok ret_1 -> true)
      else
        (match ret_1 with | Error (Invalid_argument _) -> true | _ -> false)
