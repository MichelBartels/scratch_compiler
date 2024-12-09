type binary_operator =
  | Gt
  | Lt
  | Subtract
  | Add
  | Multiply
  | Divide
  | Equals
  | And
  | Or
  | Join
  | LetterOf
  | Contains
  | Mod
[@@deriving show]

type unary_math_operator = Abs | Floor | Ceil | Sqrt | Round | Length
[@@deriving show]

type expr =
  | Argument of string
  | Variable of string
  | List of string
  | Literal of Scratch_value.t
  | BinaryOperator of binary_operator * expr * expr
  | UnaryMathOperator of unary_math_operator * expr
  | Random of {min: expr; max: expr}
  | Not of expr
  | Index of string * expr
  | IndexOf of string * expr
  | Length of string
  | Contains of {list: string; item: expr}
  | Answer
  | XPosition
  | YPosition
  | Direction
  | CostumeNumber
  | CostumeName
  | BackdropNumber
  | BackdropName
[@@deriving show]

type statement =
  | FuncCall of string * expr Parse.JsonMap.t
  | Branch of expr * statement list * statement list
  | SetVariable of string * expr
  | AddToList of string * expr
  | DeleteAllOfList of string
  | IncrVariable of string * expr
  | SetIndex of {list: string; index: expr; value: expr}
  | WhileNot of expr * statement list
  | Repeat of expr * statement list
  | Say of expr
  | SayForSeconds of {message: expr; duration: expr}
  | Think of expr
  | ThinkForSeconds of {message: expr; duration: expr}
  | SwitchCostume of expr
  | NextCostume
  | SwitchBackdrop of int
  | NextBackdrop
  | ChangeSizeBy of expr
  | Show
  | Hide
  | GoForwardBackwardLayers of expr * Layer_direction.t
  | GoToFrontBack of Layer_direction.t
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
  | Warn of string
[@@deriving show]

type code = statement list [@@deriving show]

type scratch_function = {parameters: string list; code: code} [@@deriving show]

type sprite =
  { functions: scratch_function Parse.JsonMap.t
  ; variables: Scratch_value.t Parse.JsonMap.t
  ; entry_points: code list
  ; current_costume: int
  ; costumes: Costume.t list
  ; name: string
  ; x: float
  ; y: float
  ; direction: float
  ; rotation_style: Rotation_style.t
  ; is_stage: bool }
[@@deriving show]

type program = {sprites: sprite list; globals: Scratch_value.t Parse.JsonMap.t}
[@@deriving show]
