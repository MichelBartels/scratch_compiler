(* 

   The Parsed AST 

*) 
type loc = Lexing.position 

type value = 
  | Object of loc * ((string * value) list)
  | Array of loc * (value list)
  | String of loc * string
  | Number of loc * float
  | Boolean of loc * bool
  | Null of loc

let loc_of_value = function 
    | Object (loc, _) -> loc 
    | Array (loc, _) -> loc 
    | String (loc, _) -> loc
    | Number (loc, _) -> loc
    | Boolean (loc, _) -> loc
    | Null loc -> loc


let string_of_loc loc = 
    "line " ^ (string_of_int (loc.Lexing.pos_lnum)) ^ ", " ^ 
    "position " ^ (string_of_int ((loc.Lexing.pos_cnum - loc.Lexing.pos_bol) + 1))

open Format

(*
   Documentation of Format can be found here: 
   http://caml.inria.fr/resources/doc/guides/format.en.html
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
*) 

let fstring ppf s = fprintf ppf "%s" s

let rec pp_dict ppf = function
  | [] -> fstring ppf ""
  | (k, v)::[] -> fprintf ppf "%s: %a" k pp_value v
  | (k, v)::rest -> fprintf ppf "%s: %a, %a" k pp_value v pp_dict rest
and pp_array ppf = function
  | [] -> fstring ppf ""
  | v::[] -> fprintf ppf "%a" pp_value v
  | v::rest -> fprintf ppf "%a, %a" pp_value v pp_array rest
and pp_value ppf = function 
  | Object (_, kvs) -> fprintf ppf "{%a}" pp_dict kvs
  | Array (_, vs) -> fprintf ppf "[%a]" pp_array vs
  | String (_, str) -> fprintf ppf "\"%s\"" str
  | Number (_, f) -> fstring ppf @@ string_of_float f
  | Boolean (_, b) -> fstring ppf @@ string_of_bool b
  | Null _ -> fstring ppf "null"

let print_value v = 
    let _ = pp_value std_formatter v
    in print_flush () 

let eprint_value v = 
    let _ = pp_value err_formatter v
    in print_flush () 

(* useful for degugging *) 

let string_of_value v = 
  let _ = pp_value str_formatter v
  in flush_str_formatter ()
