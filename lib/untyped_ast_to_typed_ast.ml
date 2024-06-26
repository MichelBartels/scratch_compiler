open Typed_ast

type inferred_type =
    | Argument of string * string
    | Variable of string
    | ListElement of string
    | Type of Scratch_type.primitive_type
    | Statement

module InferredTypeSet = Set.Make(struct
    type t = inferred_type
    let compare = compare
end)
module StringSet = Set.Make(String)
module ArgSet = Set.Make(struct
    type t = string * string
    let compare = compare
end)

let output_type ?function_name = function
    | Untyped_ast.Argument a -> Argument (a, match function_name with
        | Some name -> name
        | None -> failwith @@ "arguments can only be inside functions: " ^ a
    )
    | Variable v -> Variable v
    | Literal l -> Type (match l with Primitive l -> Scratch_value.get_primitive_type l | List _ -> failwith "lists are not literals")
    | BinaryOperator (op, _, _) -> Type (match op with
        | Gt -> Boolean
        | Lt -> Boolean
        | Subtract -> Float
        | Add -> Float
        | Equals -> Boolean
        | Or -> Boolean
        | Join -> String
        | LetterOf -> String
    )
    | Not _ -> Type Boolean
    | FuncCall _ -> Statement
    | Branch _ -> Statement
    | SetVariable _ -> Statement
    | AddToList _ -> Statement
    | DeleteAllOfList _ -> Statement
    | Index (l, _) -> ListElement l
    | IncrVariable _ -> Statement
    | IndexOf _ -> Type Float
    | SetIndex _ -> Statement
    | Length _ -> Type Float
    | WhileNot _ -> Statement
    | Repeat _ -> Statement
    | Say _ -> Statement
    | Ask _ -> Statement
    | Answer -> Type String

let rec tree_map f e = (f e) @ Untyped_ast.(
    let tree_map = tree_map f
    in let tree_map_list es = List.map tree_map es |> List.flatten
    in match e with
        | Argument _ -> []
        | Variable _ -> []
        | Literal _ -> []
        | BinaryOperator (_, e1, e2) -> (tree_map e1) @ (tree_map e2)
        | Not e -> tree_map e
        | FuncCall _ -> []
        | Branch (e, e1s, e2s) -> (f e) @ (tree_map_list (e1s @ e2s))
        | SetVariable (_, e) -> tree_map e
        | AddToList (_, e) -> tree_map e
        | DeleteAllOfList _ -> []
        | Index (_, e) -> tree_map e
        | IncrVariable (_, e) -> tree_map e
        | IndexOf (_, e) -> tree_map e
        | SetIndex s -> tree_map_list [s.index; s.value]
        | Length _ -> []
        | WhileNot (e, es) -> tree_map_list (e::es)
        | Repeat (e, es) -> tree_map_list (e::es)
        | Say e -> tree_map e
        | Ask e -> tree_map e
        | Answer -> []
)

let infer_var_type ?function_name v e = tree_map (function
    | SetVariable (v', e) when v' = v -> [output_type ?function_name e]
    | IncrVariable (v', _) when v' = v -> [Type Float]
    | _ -> []
) e |> InferredTypeSet.of_list

let find_vars e = tree_map (function
    | SetVariable (v, _) -> [v]
    | IncrVariable (v, _) -> [v]
    | _ -> []
) e |> StringSet.of_list |> StringSet.to_list

let infer_arg_type ?function_name arg_fn arg_name e = tree_map (function
    | FuncCall (fn, args) when fn = arg_fn -> (match Assoc_list.search arg_name args with 
        | Some e -> [output_type ?function_name e]
        | None -> failwith "function missing argument")
    | _ -> []
) e |> InferredTypeSet.of_list

let find_args e = tree_map (function
    | FuncCall (name, args) -> List.map (fun (arg, _) -> (name, arg)) args
    | _ -> []
) e |> ArgSet.of_list |> ArgSet.to_list

let infer_list_type ?function_name l e = tree_map (function
    | AddToList (l', e) when l' = l -> [output_type ?function_name e]
    | SetIndex s when s.list = l -> [output_type ?function_name s.value]
    | _ -> []
) e |> InferredTypeSet.of_list

let find_lists e = tree_map (function
    | AddToList (l, _) -> [l]
    | SetIndex s -> [s.list]
    | _ -> []
) e |> StringSet.of_list |> StringSet.to_list

type inferred_types = {
    variable_types: (string * InferredTypeSet.t) list;
    argument_types: ((string * string) * InferredTypeSet.t) list;
    list_types: (string * InferredTypeSet.t) list;
}

let infer_types_for_default_values program = {
    variable_types = List.map (
        function (k, Scratch_value.Primitive v) -> (k, InferredTypeSet.of_list [Type (Scratch_value.get_primitive_type v)]) | _ -> failwith "non-primitive values in variables"
    ) program.Untyped_ast.variables;
    argument_types = [];
    list_types = List.map (function
        | (k, Scratch_value.List xs) -> (k, InferredTypeSet.singleton (Type (Scratch_value.get_primitive_list_type xs)))
        | _ -> failwith "list values need to be of type list"
    ) program.lists
}

let infer_types_for_expr ?function_name e = {
    variable_types = List.map (fun k -> (k, infer_var_type ?function_name k e)) @@ find_vars e;
    argument_types = List.map (fun (arg_fn, arg_name) -> ((arg_fn, arg_name), infer_arg_type ?function_name arg_fn arg_name e)) @@ find_args e;
    list_types = List.map (fun k -> (k, infer_list_type ?function_name k e)) @@ find_lists e;
}

let merge_set_assoc_list d1 d2 = 
    List.map (fun (k, v) -> (k, InferredTypeSet.union (match Assoc_list.search k d2 with Some s -> s | None -> InferredTypeSet.empty) v)) d1
    @ List.filter (fun (k, _) -> not @@ List.exists (fun (k', _) -> k = k') d1) d2

let merge_inferred_types t1 t2 = {
    variable_types = merge_set_assoc_list t1.variable_types t2.variable_types;
    argument_types = merge_set_assoc_list t1.argument_types t2.argument_types;
    list_types = merge_set_assoc_list t1.list_types t2.list_types
}

let merge_inferred_types_list = List.fold_left merge_inferred_types {
    variable_types = [];
    argument_types = [];
    list_types = []
}

let infer_types program =
    let inferred_types = (infer_types_for_default_values program) ::
        List.map infer_types_for_expr program.Untyped_ast.main
        @ (List.map (fun (name, func) -> List.map (infer_types_for_expr ?function_name:(Some name)) func.Untyped_ast.statements) program.functions |> List.flatten)
    in merge_inferred_types_list inferred_types

let unify_types (k, types) cons = List.map (
                    fun (k', other_types) -> (k', if InferredTypeSet.mem (cons k) other_types
                    then let other_types = InferredTypeSet.remove (cons k) other_types
                    in InferredTypeSet.union types other_types
                    else other_types))

let unify_group selector cons types =
    let rec loop types = function
        | (k, _)::ks ->
                let var_type = match Assoc_list.search k @@ selector types with Some t -> t | None -> failwith "lost list element"
                in loop {
                    variable_types = unify_types (k, var_type) cons types.variable_types;
                    argument_types = unify_types (k, var_type) cons types.argument_types;
                    list_types = unify_types (k, var_type) cons types.list_types;
                } ks
        | [] -> types
    in loop types @@ selector types

let unify_variables = unify_group (fun x -> x.variable_types) (fun x -> Variable x)
let unify_arguments = unify_group (fun x -> x.argument_types) (fun (fname, argname) -> Argument (fname, argname))
let unify_lists = unify_group (fun x -> x.list_types) (fun x -> ListElement x)

let types program = program |> infer_types |> unify_variables |> unify_arguments |> unify_lists

let reduce_to_single_type t = if InferredTypeSet.mem (Type String) t then Scratch_type.String else if InferredTypeSet.mem (Type Float) t then Float else Boolean

let cast t e =
    let t' = match get_type e with Some t' -> t' | None -> failwith "cannot cast a statement into another type"
    in if t' = t then e
    else Cast (e, t)


let var_type types var = match Assoc_list.search var types.variable_types with Some t -> reduce_to_single_type t | None -> failwith @@ "no type inferred for: " ^ var
let arg_type types (f, a) = match Assoc_list.search (f, a) types.argument_types with Some t -> reduce_to_single_type t | None -> failwith @@ "no type inferred for: " ^ a
let list_type types l = match Assoc_list.search l types.list_types with Some t -> reduce_to_single_type t | None -> failwith @@ "no type inferred for: " ^ l

let rec convert_expr ?funname types e =
    let convert = convert_expr ?funname types
    in let var_type = var_type types
    in let arg_type = arg_type types
    in let list_type = list_type types
    in match e with
        | Untyped_ast.Argument arg -> (
            let funname = match funname with Some f -> f | None -> failwith "arg outside function"
            in Typed_ast.Argument (arg, Primitive (arg_type (funname, arg))))
        | Variable var -> Variable (var, Primitive (var_type var))
        | Literal v -> Literal v
        | BinaryOperator (op, e1, e2) -> let (t1, t2) = bin_op_input_type ?funname types e1 e2 op in BinaryOperator (op, convert e1 |> cast t1, convert e2 |> cast t2)
        | Not e -> Not(convert e |> cast (Primitive Boolean))
        | FuncCall (name, es) -> FuncCall (name, List.map (fun (k, v) -> (k, convert v |> cast (Primitive (arg_type (name, k))))) es)
        | Branch (cond, then_branch, else_branch) -> Branch (convert cond |> cast (Primitive Boolean), List.map convert then_branch, List.map convert else_branch)
        | SetVariable (name, e) -> SetVariable (name, convert e |> cast (Primitive (var_type name)))
        | AddToList (name, e) -> AddToList (name, convert e |> cast (Primitive (list_type name)))
        | DeleteAllOfList name -> DeleteAllOfList name
        | Index (name, i) -> Index (name, convert i |> cast (Primitive Float), Primitive (list_type name))
        | IncrVariable (name, e) -> IncrVariable (name, convert e |> cast (Primitive Float))
        | IndexOf (name, e) -> IndexOf (name, convert e |> cast (Primitive (list_type name)))
        | SetIndex s -> SetIndex (s.list, convert s.index |> cast (Primitive Float), convert s.value |> cast (Primitive (list_type s.list)))
        | Length l -> Length l
        | WhileNot (cond, body) -> WhileNot (convert cond |> cast (Primitive Boolean), List.map convert body)
        | Repeat (cond, body) -> Repeat (convert cond |> cast (Primitive Float), List.map convert body)
        | Say message -> Say (convert message |> cast (Primitive String))
        | Ask question -> Ask (convert question |> cast (Primitive String))
        | Answer -> Answer
and bin_op_input_type ?funname types e1 e2 = function
    | Untyped_ast.Gt -> (Primitive Float, Primitive Float)
    | Lt -> (Primitive Float, Primitive Float)
    | Subtract -> (Primitive Float, Primitive Float)
    | Add -> (Primitive Float, Primitive Float)
    | Equals ->
            let e1 = convert_expr ?funname types e1
            in let e2 = convert_expr ?funname types e2
            in let t1 = match get_type e1 with Some t1 -> t1 | None -> failwith "cannot compare statements"
            in let t2 = match get_type e2 with Some t2 -> t2 | None -> failwith "cannot compare statements"
            in (match (t1, t2) with
                | (Primitive String, _) -> (Primitive String, Primitive String)
                | (_, Primitive String) -> (Primitive String, Primitive String)
                | (Primitive Float, _) -> (Primitive Float, Primitive Float)
                | (_, Primitive Float) -> (Primitive Float, Primitive Float)
                | _ -> (Primitive Boolean, Primitive Boolean)
            )
    | Or -> (Primitive Boolean, Primitive Boolean)
    | Join -> (Primitive String, Primitive String)
    | LetterOf -> (Primitive Float, Primitive String)

let convert_variable types (n, v) = (n, Scratch_value.Primitive (Scratch_value.cast (var_type types n) v))
let convert_lists types (n, v) = (n, Scratch_value.List (match v with
    | Scratch_value.List l -> List.map (fun x -> Scratch_value.Primitive x) l |> List.map (Scratch_value.cast (list_type types n))
    | _ -> failwith "cannot cast to list"
), list_type types n)
let convert_function types (n, f) = (n, {
    parameters = List.map (fun p -> (p, Scratch_type.Primitive (arg_type types (n, p)))) f.Untyped_ast.parameters;
    statements = List.map (convert_expr ~funname:n types) f.statements;
})
let convert program =
    let types = types program in {
        variables = List.map (convert_variable types) program.Untyped_ast.variables;
        lists = List.map (convert_lists types) program.lists;
        functions = List.map (convert_function types) program.functions;
        main = List.map (convert_expr types) program.main
    }
