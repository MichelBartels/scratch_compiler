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
    | Literal of Scratch_value.t
    | BinaryOperator of binary_operator * expr * expr
    | Not of expr
    | Index of string * expr
    | IndexOf of string * expr
    | Length of string
    | Answer
[@@ deriving show]

type statement =
    | FuncCall of string * expr Parse.JsonMap.t
    | Branch of expr * statement list * statement list
    | SetVariable of string * expr
    | AddToList of string * expr
    | DeleteAllOfList of string
    | IncrVariable of string * expr
    | SetIndex of {
        list: string;
        index: expr;
        value: expr
    }
    | WhileNot of expr * (statement list)
    | Repeat of expr * (statement list)
    | Say of expr
    | Ask of expr
[@@ deriving show]

type code = statement list
[@@ deriving show]

type scratch_function = {
  parameters: string list;
  code: code;
}
[@@ deriving show]

type sprite = {
    functions: scratch_function Parse.JsonMap.t;
    variables: Scratch_value.t Parse.JsonMap.t;
    entry_points: code list;
}
[@@ deriving show]

type program = {
    sprites: sprite list;
    globals: Scratch_value.t Parse.JsonMap.t;
}
[@@ deriving show]
