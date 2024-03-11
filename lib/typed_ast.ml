module Set: sig
    type 'a t
    val contains: 'a -> 'a t -> bool
    val add: 'a -> 'a t -> 'a t
    val union: 'a t list -> 'a t
    val remove: 'a -> 'a t -> 'a t
    val from_list: 'a list -> 'a t
    val to_list: 'a t -> 'a list
    val pp: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
    val empty: 'a t
end = struct
    type 'a t = 'a list
    [@@ deriving show]
    let contains t = List.exists (fun x -> x = t)
    let add t set = if contains t set then set else t::set
    let union s = List.fold_left (List.fold_left (fun x y -> add y x)) [] s
    let remove v = List.filter (fun v' -> v' <> v)
    let from_list xs = List.fold_left (fun x y -> add y x) [] xs
    let to_list xs = xs
    let empty = []
end


(*type float_float_binary_op =
    | Add
    | Subtract

type float_boolean_binary_op =
    | Gt
    | Lt
    | Equals

type boolean_binary_op =
    | Or

type _ expr =
    | FloatArgument: string -> float expr
    | StringArgument: string -> string expr
    | BooleanArgument: string -> bool expr
    | FloatVariable: string -> float expr
    | StringVariable: string -> string expr
    | BooleanVariable: string -> bool expr
    | FloatLiteral: float -> float expr
    | StringLiteral: string -> string expr
    | BooleanLiteral: bool -> bool expr
    | FloatFloatBinaryOp: float expr * float expr * float_float_binary_op -> float expr
    | FloatBooleanBinaryOp: float expr * float expr * float_boolean_binary_op -> bool expr
    | BooleanBinaryOp: bool expr * bool expr * boolean_binary_op -> bool expr 
    | Join: string expr * string expr -> string expr
    | LetterOf: float expr * string expr -> string expr
    | Not: bool expr -> bool expr
    | FuncCall: string * (string * *)

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
