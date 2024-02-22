type number_expr =
    | Variable of string
    | Argument of string
    | Literal of float
    | Add of number_expr * number_expr
    | Subtract of number_expr * number_expr

open Format
open Pp

let rec pp_number_expr ppf = function
    | Variable var -> fprintf ppf "Variable: %s" var
    | Argument arg -> fprintf ppf "Argument: %s" arg
    | Literal lit -> fprintf ppf "Literal: %s" @@ string_of_float lit
    | Add (n1, n2) -> fprintf ppf "Add: (%a), (%a)" pp_number_expr n1 pp_number_expr n2
    | Subtract (n1, n2) -> fprintf ppf "Subtract: (%a), (%a)" pp_number_expr n1 pp_number_expr n2

type bool_expr =
    | GreaterThan of number_expr * number_expr

let pp_bool_expr ppf = function
    | GreaterThan (n1, n2) -> fprintf ppf "GreaterThan: (%a), (%a)" pp_number_expr n1 pp_number_expr n2

type statement =
    | FuncCall of string * (string * number_expr) list
    | Branch of bool_expr * statement list * statement list
    | SetVariable of string * number_expr

let pp_arg ppf (k, v) = fprintf ppf "%s: (%a)" k pp_number_expr v
let pp_args = pp_list pp_arg

let rec pp_statement ppf = function
    | FuncCall (name, args) -> fprintf ppf "FuncCall: [Name: %s Args: {%a}]" name pp_args args
    | Branch (cond, then_branch, else_branch) -> fprintf ppf "Branch: [condition: %a then: (%a) else: (%a)]" pp_bool_expr cond pp_statements then_branch pp_statements else_branch
    | SetVariable (name, value) -> fprintf ppf "SetVariable: [name: %s value {%a}]" name pp_number_expr value
and pp_statements ppf = pp_list pp_statement ppf

let print_statement = print_pp pp_statement

type function_ = {
  parameters: string list;
  statements: statement list;
}

let pp_function ppf { parameters; statements } =
    fprintf ppf "function: [parameters: [%a] statements: [%a]]" (pp_list pp_string) parameters pp_statements statements

type program = {
    functions: (string * function_) list;
    variables: (string * float) list;
    main: statement list;
}

let pp_program ppf { functions; variables; main } =
    fprintf ppf "program: [functions: [%a] main: [%a] variables: [%a]]" (pp_list (fun ppf (k, v) -> fprintf ppf "%s: %a" k pp_function v)) functions pp_statements main (pp_list (fun ppf (k, v) -> fprintf ppf "%s: %s" k (string_of_float v))) variables

let print_program = print_pp pp_program
