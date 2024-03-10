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

type scratch_type =
    | Boolean
    | Float
    | String
[@@ deriving show]

let list_to_string l = List.fold_right (fun x y -> x ^ y) l ""

let rec cast_scratch_value t v = match v with
    | Untyped_ast.NumberValue n -> (match t with
        | Boolean -> failwith "cannot cast to boolean"
        | Float -> Untyped_ast.NumberValue n
        | String -> StringValue (string_of_float n)
    )
    | BoolValue b -> (match t with
        | Boolean -> BoolValue b
        | Float -> NumberValue (if b then 1. else 0.)
        | String -> StringValue (if b then "true" else "false")
    )
    | StringValue str -> (match t with
        | Boolean -> failwith "cannot cast to boolean"
        | Float -> NumberValue (match float_of_string_opt str with
            | Some n -> n
            | None -> 0.
        )
        | String -> StringValue str
    )
    | ListValue l -> (let str = List.map (cast_scratch_value t) l |> List.map (fun x -> match x with Untyped_ast.StringValue str -> str | _ -> failwith "invalid output") |> list_to_string in match t with
        | Boolean -> failwith "cannot cast to boolean"
        | Float -> NumberValue (match float_of_string_opt str with
            | Some f -> f
            | None -> 0.
        )
        | String -> StringValue str)

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
    | Argument of string * scratch_type
    | Variable of string * scratch_type
    | Literal of Untyped_ast.scratch_value
    | BinaryOperator of Untyped_ast.binary_operator * expr * expr
    | Not of expr
    | FuncCall of string * (string * expr) list
    | Branch of expr * expr list * expr list
    | SetVariable of string * expr
    | AddToList of string * expr
    | DeleteAllOfList of string
    | Index of string * expr * scratch_type
    | IncrVariable of string * expr
    | IndexOf of string * expr
    | SetIndex of string * expr * expr
    | Length of string
    | WhileNot of expr * expr list
    | Repeat of expr * expr list
    | Say of expr
    | Ask of expr
    | Answer
    | Cast of expr * scratch_type
[@@ deriving show]

let scratch_type_of_scratch_value = Untyped_ast.(function
    | NumberValue _ -> Float
    | BoolValue _ -> Boolean
    | StringValue _ -> String
    | ListValue _ -> String
)

let get_type = function
    | Argument (_, t) -> Some t
    | Variable (_, t) -> Some t
    | Literal l -> Some (scratch_type_of_scratch_value l)
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
    parameters: (string * scratch_type) list;
    statements: expr list
}
[@@ deriving show]

type program = {
    functions: (string * scratch_function) list;
    variables: (string * Untyped_ast.scratch_value) list;
    lists: (string * Untyped_ast.scratch_value) list;
    main: expr list
}
[@@ deriving show]
