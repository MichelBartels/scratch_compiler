type expr =
    | Argument of string * Scratch_type.t
    | Variable of string * Scratch_type.t
    | Literal of Scratch_value.t
    | BinaryOperator of Untyped_ast.binary_operator * expr * expr
    | Not of expr
    | FuncCall of string * (string * expr) list
    | Branch of expr * expr list * expr list
    | SetVariable of string * expr
    | AddToList of string * expr
    | DeleteAllOfList of string
    | Index of string * expr * Scratch_type.t
    | IncrVariable of string * expr
    | IndexOf of string * expr
    | SetIndex of string * expr * expr
    | Length of string
    | WhileNot of expr * expr list
    | Repeat of expr * expr list
    | Say of expr
    | Ask of expr
    | Answer
    | Cast of expr * Scratch_type.t
[@@ deriving show]

let get_type = function
    | Argument (_, t) -> Some t
    | Variable (_, t) -> Some t
    | Literal l -> Some (Scratch_value.get_type l)
    | BinaryOperator (Gt, _, _) -> Some Boolean
    | BinaryOperator (Lt, _, _) -> Some Boolean
    | BinaryOperator (Subtract, _, _) -> Some Float
    | BinaryOperator (Add, _, _) -> Some Float
    | BinaryOperator (Equals, _, _) -> Some Boolean
    | BinaryOperator (Or, _, _) -> Some Boolean
    | BinaryOperator (Join, _, _) -> Some String
    | BinaryOperator (LetterOf, _, _) -> Some String
    | Not _ -> Some Boolean
    | FuncCall _ -> None
    | Branch _ -> None
    | SetVariable _ -> None
    | AddToList _ -> None
    | DeleteAllOfList _ -> None
    | Index (_, _, t) -> Some t
    | IncrVariable _ -> None
    | IndexOf _ -> Some Float
    | SetIndex _ -> None
    | Length _ -> Some Float
    | WhileNot _ -> None
    | Repeat _ -> None
    | Say _ -> None
    | Ask _ -> None
    | Answer -> Some String
    | Cast (_, t) -> Some t


type scratch_function = {
    parameters: (string * Scratch_type.t) list;
    statements: expr list
}
[@@ deriving show]

type program = {
    functions: (string * scratch_function) list;
    variables: (string * Scratch_value.t) list;
    lists: (string * Scratch_value.t) list;
    main: expr list
}
[@@ deriving show]
