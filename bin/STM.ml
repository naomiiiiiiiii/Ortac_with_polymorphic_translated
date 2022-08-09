include Util
type 'a ty = ..

type _ ty +=
  | Unit : unit ty
  | Bool : bool ty
  | Char : char ty
  | Int : int ty
  | Int32 : int32 ty
  | Int64 : int64 ty
  | Float : float ty
  | String : string ty
  | Bytes : bytes ty
  | Exn : exn ty
  | Option : 'a ty -> 'a option ty
  | Result : 'a ty * 'b ty -> ('a, 'b) result ty
  | List : 'a ty -> 'a list ty

type 'a ty_show = 'a ty * ('a -> string)

let unit = (Unit, fun () -> "()")
let bool = (Bool, string_of_bool)
let char = (Char, fun c -> Printf.sprintf "%C" c)
let int = (Int, string_of_int)
let int32 = (Int32, Int32.to_string)
let int64 = (Int64, Int64.to_string)
let float = (Float, Float.to_string)
let string = (String, fun s -> Printf.sprintf "%S" s)
let bytes = (Bytes, fun b -> Printf.sprintf "%S" (Bytes.to_string b))
let option spec =
  let (ty,show) = spec in
  (Option ty, QCheck.Print.option show)
let exn = (Exn, Printexc.to_string)

let show_result show_ok show_err = function
  | Ok x    -> Printf.sprintf "Ok (%s)" (show_ok x)
  | Error y -> Printf.sprintf "Error (%s)" (show_err y)

let result spec_ok spec_err =
  let (ty_ok, show_ok) = spec_ok in
  let (ty_err, show_err) = spec_err in
  (Result (ty_ok, ty_err), show_result show_ok show_err)
let list spec =
  let (ty,show) = spec in
  (List ty, QCheck.Print.list show)

type res =
  Res : 'a ty_show * 'a -> res

let show_res (Res ((_,show), v)) = show v
