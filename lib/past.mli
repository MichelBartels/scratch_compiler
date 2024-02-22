type loc = Lexing.position 
type value = 
  | Object of loc * ((string * value) list)
  | Array of loc * (value list)
  | String of loc * string
  | Number of loc * float
  | Boolean of loc * bool
  | Null of loc

val loc_of_value : value -> loc 
val string_of_loc : loc -> string 

(* printing *) 
val string_of_value : value -> string 
val print_value : value -> unit 
val eprint_value : value -> unit


