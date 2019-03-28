{
    open Parser
    open Lexing
    exception Error of string

}
(* 空白 *)
let space = [' ' '\r' '\t']+

(* 改行 *)
let newline = '\r' | '\n' | "\r\n"

(* identifier *)
let id = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*

(* 整数/実数 *)
let digits = '0' | ['1'-'9']['0'-'9']*
let fdigits = ('0' | ['1'-'9']['0'-'9']*) '.' ['0'-'9']+

(* rule *)
(* ruleの中では先に現れたほうが優先されるので注意 *)
rule read = parse
    | space             { read lexbuf } (* 空白無視 *)
    | newline           { Lexing.new_line lexbuf; read lexbuf } (* 改行無視 *)
    | '#'               { read_comment lexbuf; read lexbuf } (* コメントをパースし,そのあとに字句解析再開 *)
    | ','               { COMMA }
    | '['               { LBRACKET }
    | ']'               { RBRACKET }
    | '('               { LPAREN }
    | ')'               { RPAREN }
    | ':'               { COLON }
    (* | ';'               { SEMICOLON } *)
    | '@'               { AT }
    (* (1* 演算 *1) *)
    | '+'               { PLUS }
    (* | '-'               { MINUS } *)
    (* | '%'               { PERCENT } *)
    (* | '*'               { ASTERISK } *)
    (* | '/'               { SLASH } *)
    (* | '^'               { XOR } *)
    | "||"              { OR }
    (* | "&&"              { AND } *)
    (* | '|'               { LOR } *)
    (* | '&'               { LAND } *)
    | "=="              { EQUAL2 }
    (* | "!="              { NEQ } *)
    | '='               { EQUAL }
    | "<="              { LTE }
    (* | "<<'"             { LSHIFT } *)
    | '<'               { LT }
    | ">="              { RTE }
    (* | ">>"              { RSHIFT } *)
    | '>'               { RT }
    | "module"          { MODULE }
    | "in"              { IN }
    | "out"             { OUT }
    | "use"             { USE }
    | "node"            { NODE }
    | "init"            { INIT }
    | "true"            { TRUE }
    | "false"           { FALSE }
    | "if"              { IF }
    | "then"            { THEN }
    | "else"            { ELSE }
    | "last"            { LAST }
    | "function"        { FUNCTION }
    | id                { ID (Lexing.lexeme lexbuf) }
    | fdigits           { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | digits            { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | eof               { EOF }
    | _                 { raise (Error ("Unexpected Char: " ^ Lexing.lexeme lexbuf ^ " line: " ^ (string_of_int lexbuf.lex_curr_p.pos_lnum) ^ " column: " ^ (string_of_int (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)))) }
and read_comment = parse
    | '\n'              { new_line lexbuf }
    | eof               { () }
    | _                 { read_comment lexbuf }
