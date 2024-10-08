type block =
  | Constant of Scratch_value.primitive_value
  | Variable of string
  | Argument of {name: string}
  | ProceduresDefinition of {next: block option; prototype: block}
  | ProceduresPrototype of {parameters: block Parse.JsonMap.t; proccode: string}
  | BinaryOperator of
      {operator: Untyped_ast.binary_operator; arg1: block; arg2: block}
  | Not of {arg: block}
  | ProceduresCall of
      {next: block option; inputs: (string * block) list; proccode: string}
  | Start of {next: block option}
  | IfThenElse of
      { next: block option
      ; condition: block
      ; then_branch: block option
      ; else_branch: block option }
  | SetVariable of {next: block option; variable: string; value: block}
  | AddToList of {next: block option; list: string; item: block}
  | DeleteAllOfList of {next: block option; list: string}
  | NumOfList of {list: string; item: block}
  | ChangeVariableBy of {next: block option; value: block; variable: string}
  | ItemOfList of {list: string; index: block}
  | ReplaceItemOfList of
      {next: block option; list: string; index: block; item: block}
  | LengthOfList of {list: string}
  | RepeatUntil of {next: block option; condition: block; body: block option}
  | Repeat of {next: block option; count: block; body: block option}
  | Say of {next: block option; message: block}
  | Ask of {next: block option; question: block}
  | Answer
  | SetX of {next: block option; x: block}
  | SetY of {next: block option; y: block}
  | ChangeXBy of {next: block option; x: block}
  | ChangeYBy of {next: block option; y: block}
  | GoToXY of {next: block option; x: block; y: block}
  | GoTo of {next: block option; target: block}
  | GoToMenu of string
  | TurnRight of {next: block option; degrees: block}
  | TurnLeft of {next: block option; degrees: block}
  | MoveSteps of {next: block option; steps: block}
  | XPosition
  | YPosition
  | Direction
  | GlideToXY of {next: block option; x: block; y: block; duration: block}
  | GlideTo of {next: block option; target: block; duration: block}
  | GlideToMenu of string
  | PointTowards of {next: block option; target: block}
  | PointTowardsMenu of string
  | IfOnEdgeBounce of {next: block option}
  | SetRotationStyle of {next: block option; style: Rotation_style.t}
[@@deriving show]

type variables = Scratch_value.t Parse.JsonMap.t [@@deriving show]

type costume = Costume.t [@@deriving show]

type sprite =
  { variables: variables
  ; blocks: block list
  ; current_costume: int
  ; costumes: costume list
  ; name: string
  ; x: float
  ; y: float
  ; direction: float
  ; rotation_style: Rotation_style.t }
[@@deriving show]

type program = {sprites: sprite list; globals: variables} [@@deriving show]
