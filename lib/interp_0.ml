open Ast
open Assoc_list
open Pp
open Format

type state = {
    variables: (string * float) list;
    arguments: (string * float) list;
}

let pp_var ppf (str, n) =
    fprintf ppf "%s: %s" str (string_of_float n)
let pp_vars = pp_list pp_var

let pp_state ppf { variables; arguments } =
    fprintf ppf "state: [variables: %a arguments: %a]" pp_vars variables pp_vars arguments
let print_state = print_pp pp_state


let rec interp_number_expr program state = function
    | Variable var -> (match search_assoc_list var state.variables with
        | Some v -> v
        | None -> failwith "could not find variable")
    | Argument arg -> (match search_assoc_list arg state.arguments with
        | Some v -> v
        | None -> failwith @@ "could not find argument: " ^ arg)
    | Literal n -> n
    | Add (n1, n2) -> (interp_number_expr program state n1) +. (interp_number_expr program state n2)
    | Subtract (n1, n2) -> (interp_number_expr program state n1) -. (interp_number_expr program state n2)

let interp_bool_expr program state = function
    | GreaterThan (n1, n2) -> (interp_number_expr program state n1) > (interp_number_expr program state n2)

let rec interp_statements program state = function
    | (FuncCall (f, args))::statements -> (match search_assoc_list f program.functions with
        | Some f -> (let vars = (interp_statements program { variables = state.variables; arguments = List.map (fun (k, v) -> (k, interp_number_expr program state v)) args } f.statements).variables (* might add parameter check *)
        in interp_statements program { variables = vars; arguments = state.arguments } statements)
        | None -> failwith "could not find function"
    )
    | (Branch (cond, then_branch, else_branch))::statements ->
        (let vars = if interp_bool_expr program state cond then (interp_statements program state then_branch).variables else (interp_statements program state else_branch).variables
        in interp_statements program { variables = vars; arguments = state.arguments } statements)
    | (SetVariable (name, value))::statements ->
        (let vars = update_assoc_list name (interp_number_expr program state value) state.variables
        in interp_statements program { variables = vars; arguments = state.arguments } statements)
    | [] -> state

let interp_program (program: Ast.program) =
    let state = { arguments = []; variables = program.variables }
    in interp_statements program state program.main

