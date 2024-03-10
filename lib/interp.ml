open Typed_ast

module type State = sig
    type t
    (*val (let: t -> (Untyped_ast.scratch_value option -> Untyped_ast.scratch_value option) -> *)
end
type state = {
    variables: (string * Untyped_ast.scratch_value) list;
    lists: (string * Untyped_ast.scratch_value) list;
    arguments: (string * Untyped_ast.scratch_value) list;
}
module State = struct
    type t = {
        variables: (string * Untyped_ast.scratch_value) list;
        lists: (string * Untyped_ast.scratch_value) list;
        arguments: (string * Untyped_ast.scratch_value) list;
    }
    (*let return program = {
        variables = program.variables;
        lists = program.lists;
        arguments = 
    }
    val interp: *)
end

let assert_some = function | Some s -> s | None -> failwith "internal error"

let rec interp_expr program state =
    let interp_expr = interp_expr program in
    let (let*) (state, expr) f = let (state, v) = interp_expr state expr in (state, Some (f (assert_some v))) in function
    | Argument (arg, _) -> (match Assoc_list.search arg state.arguments with
        | Some v -> (state, Some v)
        | None -> failwith @@ "could not find argument: " ^ arg)
    | Variable (var, _) -> (match Assoc_list.search var state.variables with
        | Some v -> (state, Some v)
        | None -> failwith "could not find variable")
    | BinaryOperator (op, e1, e2) ->
            let (state, v1) = interp_expr state e1
            in let (state, v2) = interp_expr state e2
            in (match (v1, v2, op) with
                | (Some (NumberValue v1), Some (NumberValue v2), Gt) -> (state, Some (Untyped_ast.BoolValue (v1 > v2)))
                | (Some (NumberValue v1), Some (NumberValue v2), Lt) -> (state, Some (Untyped_ast.BoolValue (v1 < v2)))
                | (Some (NumberValue v1), Some (NumberValue v2), Equals) -> (state, Some (Untyped_ast.BoolValue (v1 = v2)))
                | (Some (NumberValue v1), Some (NumberValue v2), Subtract) -> (state, Some (Untyped_ast.NumberValue (v1 -. v2)))
                | (Some (NumberValue v1), Some (NumberValue v2), Add) -> (state, Some (Untyped_ast.NumberValue (v1 -. v2)))
                | (Some (BoolValue v1), Some (BoolValue v2), Or) -> (state, Some (BoolValue (v1 || v2)))
                | (Some (StringValue v1), Some (StringValue v2), Join) -> (state, Some (StringValue (v1 ^ v2)))
                | (Some (NumberValue v1), Some (StringValue v2), LetterOf) -> (state, Some (StringValue (String.make 1 v2.[int_of_float v1])))
                | _ -> failwith "invalid input for binary op"
            )
    |  Not e ->
            let* v = (state, e) in match v with BoolValue v -> Untyped_ast.BoolValue (not v) | _ -> failwith "not requires bool as input"
