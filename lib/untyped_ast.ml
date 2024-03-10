type scratch_value =
    | NumberValue of float
    | BoolValue of bool
    | StringValue of string
    | ListValue of scratch_value list
[@@ deriving show]

type binary_operator =
    | Gt
    | Lt
    | Subtract
    | Add
    | Equals
    | Or
    | Join
    | LetterOf
[@@ deriving show]

type expr =
    | Argument of string
    | Variable of string
    | Literal of scratch_value
    | BinaryOperator of binary_operator * expr * expr
    | Not of expr
    | FuncCall of string * (string * expr) list
    | Branch of expr * expr list * expr list
    | SetVariable of string * expr
    | AddToList of string * expr
    | DeleteAllOfList of string
    | Index of string * expr
    | IncrVariable of string * expr
    | IndexOf of string * expr
    | SetIndex of {
        list: string;
        index: expr;
        value: expr
    }
    | Length of string
    | WhileNot of expr * (expr list)
    | Repeat of expr * (expr list)
    | Say of expr
    | Ask of expr
    | Answer
[@@ deriving show]

type function_ = {
  parameters: string list;
  statements: expr list;
}
[@@ deriving show]

type program = {
    functions: (string * function_) list;
    variables: (string * scratch_value) list;
    lists: (string * scratch_value) list;
    main: expr list;
}
[@@ deriving show]
