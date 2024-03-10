type loc = Lexing.position 
type value = 
  | Object of loc * ((string * value) list)
  | Array of loc * (value list)
  | String of loc * string
  | Number of loc * float
  | Boolean of loc * bool
  | Null of loc
[@@ deriving show]
