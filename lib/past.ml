(* 

   The Parsed AST 

*) 
type loc = Lexing.position 

let string_of_loc loc = 
    "line " ^ (string_of_int (loc.Lexing.pos_lnum)) ^ ", " ^ 
    "position " ^ (string_of_int ((loc.Lexing.pos_cnum - loc.Lexing.pos_bol) + 1))

let pp_loc ppf loc = Format.fprintf ppf "%s" (string_of_loc loc)

type value = 
  | Object of loc * ((string * value) list)
  | Array of loc * (value list)
  | String of loc * string
  | Number of loc * float
  | Boolean of loc * bool
  | Null of loc
[@@ deriving show]
