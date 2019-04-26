(* type representng type *)
type t = 
    TUnit
  | TBool
  | TChar
  | TInt
  | TFloat
  | TTuple of t list
  | TList of t
[@@deriving show]

let of_string = function
  | TUnit  -> "void"
  | TBool  -> "bool"
  | TChar  -> "char"
  | TInt   -> "int"
  | TFloat -> "float"
  | TTuple _ -> "tuple"
  | TList _ -> "list"
