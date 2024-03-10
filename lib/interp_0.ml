(*open Ast
open Assoc_list

type state = {
    variables: (string * scratch_value) list;
    arguments: (string * scratch_value) list;
}
[@@ deriving show]

let rec interp_expr program state = function
    | Variable var -> (match Assoc_list.search var state.variables with
        | Some v -> Some v
        | None -> failwith "could not find variable")
    | Argument arg -> (match Assoc_list.search arg state.arguments with
        | Some v -> Some v
        | None -> failwith @@ "could not find argument: " ^ arg)
    | Literal l -> Some l
    | BinaryOperator (op, n1, n2) -> 
    | (FuncCall (f, args))::statements -> (match Assoc_list.search f program.functions with
        | Some f -> (let vars = (interp_statements program { variables = state.variables; arguments = List.map (fun (k, v) -> (k, interp_expr program state v)) args } f.statements).variables (* might add parameter check *)
        in interp_statements program { variables = vars; arguments = state.arguments } statements)
        | None -> failwith "could not find function"
    )
    | (Branch (cond, then_branch, else_branch))::statements ->
        (let vars = if (match interp_expr program state cond with
            | BoolValue b -> b
            | _ -> failwith "if only accepts booleans as conditions"
        ) then (interp_statements program state then_branch).variables else (interp_statements program state else_branch).variables
        in interp_statements program { variables = vars; arguments = state.arguments } statements)
    | (SetVariable (name, value))::statements ->
        (let vars = update_assoc_list name (interp_expr program state value) state.variables
        in interp_statements program { variables = vars; arguments = state.arguments } statements)
    | (Push (name, value))::statements -> 
        (let list = ListValue ((interp_expr program state value)::(match Assoc_list.search name state.variables with Some (ListValue xs) -> xs | _ -> failwith "updating value that does not exist or is not a list"))
        in let vars = update_assoc_list name list state.variables
        in interp_statements program { variables = vars; arguments = state.arguments } statements)
    | [] -> state

let interp_program (program: program) =
    let state = { arguments = []; variables = program.variables }
    in interp_statements program state program.main
*)
