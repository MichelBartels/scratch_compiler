(* Exactly the front end provided in Slang interpreter *)

open Lexing 

let error file action s = 
    Errors.complain ("\nERROR in " ^ file ^ " with " ^ action ^ " : " ^ s ^ "\n") 

let peek m e pp = if Option.verbose_front then print_string (m ^ ":\n" ^ (if Option.verbose_tree then Pptree.pp_no_bracket else (fun x -> x)) (pp e)  ^ "\n") else () 

let parse_error file lexbuf = 
    let pos = lexbuf.lex_curr_p in 
    let line = string_of_int (pos.pos_lnum) in 
    let pos = string_of_int ((pos.pos_cnum - pos.pos_bol) + 1) in 
        error file "parsing" ("at line " ^ line ^ " position " ^ pos)

 (* initialize lexer *) 
let init_lexbuf file =
   let in_chan = try open_in file 
                 with _ -> error file "initialize lexer" ("can't open file " ^ file) 
  in let lexbuf = from_channel in_chan 
  in let _ = lexbuf.lex_curr_p <- { pos_fname = file; pos_lnum = 1; pos_bol = 0; pos_cnum = 0; }
  in (file, lexbuf) 

 (* parse input file *) 
let parse (file, lexbuf) = 
    let e = try Parser.main Lexer.token lexbuf 
            with Parsing.Parse_error -> parse_error file lexbuf 
    in let _ = peek "Parsed result" e Past.show_value 
    in (file, e) 

let front_end file = (match (parse (init_lexbuf file)) with (_, v) -> v) |> Past_to_blocks.convert |> Blocks_to_untyped_ast.convert |> Untyped_ast_to_typed_ast.convert
