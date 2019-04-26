%{
    open Syntax
    open Type
%}


(* 予約後 *)
%token MODULE IN OUT USE NODE INIT TRUE FALSE IF THEN ELSE LAST FUNCTION
(* 括弧 *)
%token LBRACKET RBRACKET LPAREN RPAREN
(* 記号 *)
%token COMMA COLON AT(* SEMICOLON *)
(* 演算 *)
%token EQUAL PLUS EQUAL2 OR LTE LT RTE RT(* MINUS PERCENT ASTERISK SLASH XOR AND LOR LAND NEQ LSHIFT RSHIFT *)
(* 識別子 *)
%token <string> ID
(* 数値 *)
%token <float> FLOAT
%token <int> INT
(* その他 *)
%token EOF

 (* 構文解析開始地点 *)
 %start <Syntax.ast> prog_module

(* 下のほうが優先順位が高い *)
%right prec_if
%left  OR
%left  EQUAL2
%left  LTE LT RTE RT
%left  PLUS

%%

prog_module:
  | MODULE id = ID                                    (* module hoge *)
    IN innodes = separated_list(COMMA,id_and_type)    (* in node1:Bool , node2:Int, ...*)
    OUT outnodes = separated_list(COMMA,id_and_type)  (* out node1:Int, ... *)
    usemodule = option(USE modules = separated_list(COMMA,ID) { modules })
    defs = nonempty_list(definition)
    EOF
    { (* return Syntax.program *)
      {
        module_id = id;
        in_nodes = innodes;
        out_nodes = outnodes;
        use = usemodule;
        definitions = defs;
      }
    }


id_and_type: (* example  v:Int *)
  | id = ID COLON t = type_specific { (id,t) } (* (Syntax.id, Type.t) *)

  (* type definition. Type.t *)
type_specific: (* when you want to know return type, see data type `t` in type.ml *)
  | t = prim_type_specific { t } (* Primitive Type *)

  (* Type.t *)
prim_type_specific: (* see Type.ml *)
  | t = ID
    {
      match t with
      | "Unit" -> TUnit
      | "Bool" -> TBool
      | "Char" -> TChar
      | "Int"  -> TInt
      | "Float"-> TFloat
      | _ -> assert false
  }


(* definition *)
definition:
      | NODE init = option(INIT LBRACKET ie = init_expr RBRACKET { ie }) (* node init[0] *)
        idt = id_and_type EQUAL e = expr
        { Node(idt,init,e) }
      | FUNCTION id = ID LPAREN args = separated_list(COMMA,id_and_type) RPAREN COLON t = type_specific EQUAL e = expr 
        { let args_id , args_type = List.split args in Fun((id,t,args_id,args_type),e) }

(* Type.expr *)
expr:
      | constant        { EConst($1) }
      | id = ID         { Eid(id) }
      | id = ID AT a = annotation { EAnnot(id,a) }
      | expr binop expr { Ebin($2,$1,$3) }
      | IF c = expr THEN  a = expr ELSE b = expr %prec prec_if (* if-expressionの優先度を下げる *)
    { Eif(c,a,b) }
      | LPAREN expr RPAREN { $2 } (* TODO 括弧だけど正しく動くのかは不明 *)
      (* TODO 関数適用 *)
      | id = ID LPAREN args = args RPAREN  { EApp(id,args) }



annotation:
      | LAST { ALast }

args:
      | separated_list(COMMA,expr)  { $1 }

(* ----- initial value of each node ----- *)
init_expr:
      | constant        { EConst($1) }
      | id = ID LPAREN args = init_args RPAREN  { EApp(id,args) }

init_args:
      | separated_list(COMMA,init_expr) { $1 }

(* ------------------------------ *)

%inline
binop:
      | PLUS    { BAdd }
      | EQUAL2  { BEq }
      | OR      { BOr }
      | LTE     { BLte }
      | LT      { BLt }
      | RTE     { BRte }
      | RT      { BRt }

(* node内に現れる数値 *)
constant:
      | TRUE    { CBool(true) }
      | FALSE   { CBool(false) }
      | INT     { CInt($1) }
      | FLOAT   { CFloat($1) }
