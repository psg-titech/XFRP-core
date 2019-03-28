type id = string
[@@deriving show]
type moduleid = string
[@@deriving show]
type id_and_type = id * Type.t
[@@deriving show]
type annot = ALast
[@@deriving show]

(* node内に現れる数値 *)
type const = 
  | CUnit (* これ、使われることあるんか...? *)
  | CBool of bool
  | CInt of int
  | CFloat of float
[@@deriving show]

type binop = 
  | BAdd
  | BEq
  | BOr
  | BLte
  | BLt
  | BRte
  | BRt
[@@deriving show]

type expr = 
  | EConst of const
  | Eid of id 
  | EAnnot of id * annot
  | Ebin of binop * expr * expr
  | Eif of expr * expr * expr
  | EApp of id * expr list
[@@deriving show]

type function_body = id list * expr (* arguments and body *)
[@@deriving show]

type definition = 
  | Node of id_and_type (* node id and type *) * expr option (* init *) * expr (* body *)
  | Const of id_and_type * expr
  | Fun of (id (* function name *) * (Type.t list (* arguments type *) * Type.t (* return type *))) * function_body
[@@deriving show]


(* 抽象構文木のroot *)
type ast = {
  module_id: moduleid;
  in_nodes : id_and_type list;    (* input node is a list of typed id *)
  out_nodes : id_and_type list;   (* output node is a list of typed id *)
  use : moduleid list option;
  definitions : definition list;    (* This includes out_nodes, but not include input_nodes *)
}

(* ---------------------------------------------------------------------------------------- *)

(* AST出力用 *)
let string_of_program program =
  let moduleid = "module_id -> " ^ program.module_id in
  let inid = "in -> " ^ (String.concat "," (List.map show_id_and_type program.in_nodes)) in
  let outid = "out -> " ^ (String.concat "," (List.map show_id_and_type program.out_nodes)) in
  let usemod = "use -> " ^ (match program.use with 
                                | Some lst -> (String.concat "," (List.map show_moduleid lst))
                                | None -> "No Module is Used") in
  let defstring = "defs ->\nDefinition:\n" ^ (String.concat "\n\nDefinition:\n" (List.map show_definition program.definitions)) in
  moduleid ^ "\n" ^ inid ^ "\n" ^ outid ^ "\n" ^ usemod ^ "\n" ^ defstring
