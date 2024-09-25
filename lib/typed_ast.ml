type expr =
  | Argument of string * Scratch_type.t
  | Variable of string * Scratch_type.t
  | Literal of Scratch_value.t
  | BinaryOperator of Untyped_ast.binary_operator * expr * expr
  | Not of expr
  | Index of string * expr * Scratch_type.t
  | IndexOf of string * expr
  | Length of string
  | Answer
  | XPosition
  | YPosition
  | Direction
  | Cast of expr * Scratch_type.t
[@@deriving show]

type statement =
  | FuncCall of string * expr Parse.JsonMap.t
  | Branch of expr * statement list * statement list
  | SetVariable of string * expr
  | AddToList of string * expr
  | DeleteAllOfList of string
  | IncrVariable of string * expr
  | SetIndex of string * expr * expr
  | WhileNot of expr * statement list
  | Repeat of expr * statement list
  | Say of expr
  | Ask of expr
  | SetX of expr
  | SetY of expr
  | ChangeX of expr
  | ChangeY of expr
  | GoToXY of {x: expr; y: expr}
  | GoTo of string
  | TurnRight of expr
  | TurnLeft of expr
  | MoveSteps of expr
  | GlideToXY of {x: expr; y: expr; duration: expr}
  | GlideTo of {target: string; duration: expr}
  | PointTowards of string
  | IfOnEdgeBounce
  | SetRotationStyle of Rotation_style.t
[@@deriving show]

let get_type = function
  | Argument (_, t) ->
      t
  | Variable (_, t) ->
      t
  | Literal l ->
      Scratch_value.get_type l
  | BinaryOperator (Gt, _, _) ->
      Primitive Boolean
  | BinaryOperator (Lt, _, _) ->
      Primitive Boolean
  | BinaryOperator (Subtract, _, _) ->
      Primitive Float
  | BinaryOperator (Add, _, _) ->
      Primitive Float
  | BinaryOperator (Equals, _, _) ->
      Primitive Boolean
  | BinaryOperator (Or, _, _) ->
      Primitive Boolean
  | BinaryOperator (Join, _, _) ->
      Primitive String
  | BinaryOperator (LetterOf, _, _) ->
      Primitive String
  | Not _ ->
      Primitive Boolean
  | Index (_, _, t) ->
      t
  | IndexOf _ ->
      Primitive Float
  | Length _ ->
      Primitive Float
  | Answer ->
      Primitive String
  | XPosition ->
      Primitive Float
  | YPosition ->
      Primitive Float
  | Direction ->
      Primitive Float
  | Cast (_, t) ->
      t

type code = statement list [@@deriving show]

type scratch_function = {parameters: (string * Scratch_type.t) list; code: code}
[@@deriving show]

type sprite =
  { functions: scratch_function Parse.JsonMap.t
  ; variables: (Scratch_value.t * Scratch_type.primitive_type) Parse.JsonMap.t
  ; entry_points: code list
  ; current_costume: int
  ; costumes: Costume.t list
  ; name: string
  ; x: float
  ; y: float
  ; direction: float
  ; rotation_style: Rotation_style.t }
[@@deriving show]

type program =
  { sprites: sprite list
  ; globals: (Scratch_value.t * Scratch_type.primitive_type) Parse.JsonMap.t }
[@@deriving show]
