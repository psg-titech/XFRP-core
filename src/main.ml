open Lexing

exception CommandError of string
exception CompileError of string

module S = Set.Make(String);;
module M = Map.Make(String);;

(* input/output filename *)
let output_file = ref None
let input_file  = ref None

(* Analyze command line argument *)
let speclist = [ (* tuple of (key=string,spec,doc=string) *)
  ("-o", Arg.String(fun s -> output_file:=Some(s)), "[file] Write Output file");
]


let compile in_c = 
  let lexbuf = from_channel in_c in
  try
    let ast : Syntax.ast= Parser.prog_module Lexer.read lexbuf in (* get AST. See prog_module in parser.mly and read rule in lexer.mll *)
    ()
  with
    | Lexer.Error msg -> raise (CompileError("Lexing error: " ^ msg))
    | Parser.Error -> let pos = lexbuf.lex_curr_p in raise (CompileError (Printf.sprintf "Syntax error at Line %d, Char %d." pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)))
  (* program : AST*) 


let main () =
  Arg.parse speclist (fun s -> input_file := Some(s)) "Usage:";
  try
    let input = open_in (match !input_file with (* input::in_channel *)
                        | Some s -> s
                        | None -> raise (CommandError "Input file is not specified.")) in
    compile input
  with
  | CommandError msg ->
      Printf.eprintf "Command Error: %s" msg;
  ()

let () = main()
