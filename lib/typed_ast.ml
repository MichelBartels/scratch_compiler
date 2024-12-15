type expr =
  | Argument of string * Scratch_type.t
  | Variable of string * Scratch_type.t
  | List of string * Scratch_type.t
  | Literal of Scratch_value.t
  | BinaryOperator of Untyped_ast.binary_operator * expr * expr
  | UnaryMathOperator of Untyped_ast.unary_math_operator * expr
  | Random of {min: expr; max: expr}
  | Not of expr
  | Index of string * expr * Scratch_type.t
  | IndexOf of string * expr
  | ListLength of string
  | StringLength of expr
  | Contains of {list: string; item: expr}
  | Answer
  | TouchesCursor
  | XPosition
  | YPosition
  | MouseX
  | MouseY
  | SensingOf of {obj: string; property: Sensing_property.t}
  | Direction
  | Cast of expr * Scratch_type.t
  | CostumeNumber
  | CostumeName
  | BackdropNumber
  | BackdropName
[@@deriving show]

type statement =
  | FuncCall of string * expr Parse.JsonMap.t
  | Broadcast of string
  | BroadcastAndWait of string
  | Branch of expr * statement list * statement list
  | SetVariable of string * expr
  | AddToList of string * expr
  | DeleteAllOfList of string
  | DeleteOfList of string * expr
  | InsertAtList of {list: string; index: expr; item: expr}
  | IncrVariable of string * expr
  | SetIndex of string * expr * expr
  | WhileNot of expr * statement list
  | Repeat of expr * statement list
  | Forever of statement list
  | Say of expr
  | SayForSeconds of {message: expr; duration: expr}
  | Think of expr
  | ThinkForSeconds of {message: expr; duration: expr}
  | SwitchCostume of expr
  | NextCostume
  | SwitchBackdrop of expr
  | NextBackdrop
  | ChangeSizeBy of expr
  | SetSizeTo of expr
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

let get_type = function
  | Argument (_, t) ->
      t
  | Variable (_, t) ->
      t
  | List (_, t) ->
      t
  | Literal l ->
      Scratch_value.get_type l
  | BinaryOperator (Gt, _, _) ->
      Primitive Boolean
  | Random _ ->
      Primitive Float
  | BinaryOperator (Lt, _, _) ->
      Primitive Boolean
  | BinaryOperator (Subtract, _, _) ->
      Primitive Float
  | BinaryOperator (Add, _, _) ->
      Primitive Float
  | BinaryOperator (Multiply, _, _) ->
      Primitive Float
  | BinaryOperator (Divide, _, _) ->
      Primitive Float
  | BinaryOperator (Equals, _, _) ->
      Primitive Boolean
  | BinaryOperator (Or, _, _) ->
      Primitive Boolean
  | BinaryOperator (And, _, _) ->
      Primitive Boolean
  | BinaryOperator (Join, _, _) ->
      Primitive String
  | BinaryOperator (LetterOf, _, _) ->
      Primitive String
  | BinaryOperator (Contains, _, _) ->
      Primitive Boolean
  | BinaryOperator (Mod, _, _) ->
      Primitive Float
  | UnaryMathOperator _ ->
      Primitive Float
  | Not _ ->
      Primitive Boolean
  | Index (_, _, t) ->
      t
  | IndexOf _ ->
      Primitive Float
  | ListLength _ ->
      Primitive Float
  | StringLength _ ->
      Primitive Float
  | Contains _ ->
      Primitive Boolean
  | Answer ->
      Primitive String
  | TouchesCursor ->
      Primitive Boolean
  | XPosition ->
      Primitive Float
  | YPosition ->
      Primitive Float
  | MouseX ->
      Primitive Float
  | MouseY ->
      Primitive Float
  | SensingOf _ ->
      Primitive String
  | Direction ->
      Primitive Float
  | Cast (_, t) ->
      t
  | CostumeNumber ->
      Primitive Float
  | CostumeName ->
      Primitive String
  | BackdropNumber ->
      Primitive Float
  | BackdropName ->
      Primitive String

type code = statement list [@@deriving show]

type scratch_function = {parameters: (string * Scratch_type.t) list; code: code}
[@@deriving show]

type sprite =
  { functions: scratch_function Parse.JsonMap.t
  ; variables: (Scratch_value.t * Scratch_type.primitive_type) Parse.JsonMap.t
  ; entry_points: code list
  ; broadcasts: code list Parse.JsonMap.t
  ; on_clicks: code list
  ; current_costume: int
  ; costumes: Costume.t list
  ; name: string
  ; x: float
  ; y: float
  ; direction: float
  ; rotation_style: Rotation_style.t
  ; is_stage: bool
  ; visible: bool }
[@@deriving show]

type program =
  { sprites: sprite list
  ; globals: (Scratch_value.t * Scratch_type.primitive_type) Parse.JsonMap.t }
[@@deriving show]
