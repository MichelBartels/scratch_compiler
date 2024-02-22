(* File lexer.mll *)
{
  open Parser
  open Lexing 

(* next_line copied from  Ch. 16 of "Real Workd Ocaml" *) 
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

}

let newline = ('\010' | "\013\010" )
let str_reg_exp = '"' ([^ '"'] | "\\\"")* '"'
let number_reg_exp = '-'? ['0'-'9']+ ('.' ['0'-'9']+)?

rule token = parse
  | [' ' '\t']     { token lexbuf }     (* skip blanks *)
  | eof            { EOF }
  | number_reg_exp { NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
  | "[" { LSQUAREBRACKET }
  | "]" { RSQUAREBRACKET }
  | "{" { LCURLYBRACKET }
  | "}" { RCURLYBRACKET }
  | "," { COMMA }
  | ":" { COLON }
  | "true" { TRUE }
  | "false" { FALSE }
  | "null" { NULL }
  | str_reg_exp { STRING (let str = Lexing.lexeme lexbuf in String.sub str 1 (String.length str - 2)) }
  | newline { next_line lexbuf; token lexbuf } 
  | eof { EOF }
  | _ { Errors.complain ("Lexer : Illegal character " ^ (Char.escaped(Lexing.lexeme_char lexbuf 0))) }
