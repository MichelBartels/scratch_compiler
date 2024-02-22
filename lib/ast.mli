type number_expr =
    | Variable of string
    | Argument of string
    | Literal of float
    | Add of number_expr * number_expr
    | Subtract of number_expr * number_expr

type bool_expr =
    | GreaterThan of number_expr * number_expr

type statement =
    | FuncCall of string * (string * number_expr) list
    | Branch of bool_expr * statement list * statement list
    | SetVariable of string * number_expr

type function_ = {
    parameters: string list;
    statements: statement list;
}

type program = {
    functions: (string * function_) list;
    variables: (string * float) list;
    main: statement list;
}

val print_program: program -> unit
val print_statement: statement -> unit
