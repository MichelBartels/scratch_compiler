open Typed_ast

type inferred_type =
  | Argument of string * string
  | Variable of string
  | Type of Scratch_type.primitive_type
[@@deriving show]

module InferredTypeSet = Set.Make (struct
  type t = inferred_type

  let compare = compare
end)

let show_inferred_type_set s =
  InferredTypeSet.fold (fun x acc -> show_inferred_type x ^ ", " ^ acc) s ""

module StringSet = Set.Make (String)

module ArgSet = Set.Make (struct
  type t = string * string

  let compare = compare
end)

let output_type ?function_name = function
  | Untyped_ast.Argument a ->
      Argument
        ( (match function_name with
          | Some name -> name
          | None -> failwith @@ "arguments can only be inside functions: " ^ a),
          a )
  | Variable v -> Variable v
  | Literal l ->
      Type
        (match l with
        | Primitive l -> Scratch_value.get_primitive_type l
        | List _ -> failwith "lists are not literals")
  | BinaryOperator (op, _, _) ->
      Type
        (match op with
        | Gt -> Boolean
        | Lt -> Boolean
        | Subtract -> Float
        | Add -> Float
        | Equals -> Boolean
        | Or -> Boolean
        | Join -> String
        | LetterOf -> String)
  | Not _ -> Type Boolean
  | Index (l, _) -> Variable l
  | IndexOf _ -> Type Float
  | Length _ -> Type Float
  | Answer -> Type String

let statement_entrypoint_map f sprites =
  List.map
    (fun sprite -> List.map (List.map f) sprite.Untyped_ast.entry_points)
    sprites
  |> List.flatten |> List.flatten |> List.flatten

let statement_function_map f sprites =
  let rec map_function f stmt =
    f stmt
    @ ((match stmt with
       | Untyped_ast.Branch (_, then_branch, else_branch) ->
           List.map (map_function f) then_branch
           @ List.map (map_function f) else_branch
       | Untyped_ast.WhileNot (_, body) -> List.map (map_function f) body
       | Untyped_ast.Repeat (_, body) -> List.map (map_function f) body
       | _ -> [])
      |> List.flatten)
  in
  List.map
    (fun sprite ->
      (Parse.StringMap.bindings sprite.Untyped_ast.functions
      |> List.map (fun (key, fun_) ->
             List.map (map_function (f (Some (key)))) fun_.Untyped_ast.code
        ))
        @ List.map (List.map (map_function (f None))) sprite.Untyped_ast.entry_points
    )
    sprites
  |> List.flatten |> List.flatten |> List.flatten

let statement_map f sprites =
  statement_function_map (fun key -> f ?function_name:key) sprites
  @ statement_entrypoint_map f sprites

let infer_var_type v e =
  statement_map
    (fun ?function_name -> function
      | SetVariable (v', e) when v' = v -> [ output_type ?function_name e ]
      | IncrVariable (v', _) when v' = v -> [ Type Float ]
      | AddToList (v', e) when v' = v -> [ output_type ?function_name e ]
      | SetIndex s when s.list = v -> [ output_type ?function_name s.value ]
      | _ -> [])
    e
  |> InferredTypeSet.of_list

let find_vars e =
  statement_map
    (fun ?function_name ->
      let _ = function_name in
      function
      | SetVariable (v, _) -> [ v ]
      | IncrVariable (v, _) -> [ v ]
      | AddToList (l, _) -> [ l ]
      | SetIndex s -> [ s.list ]
      | _ -> [])
    e
  |> StringSet.of_list |> StringSet.to_list

let infer_arg_type arg_fn arg_name e =
  statement_map
    (fun ?function_name -> function
      | FuncCall (fn, args) when fn = arg_fn -> (
          match Parse.StringMap.find_opt arg_name args with
          | Some e -> [ output_type ?function_name e ]
          | None -> failwith "function missing argument")
      | _ -> [])
    e
  |> InferredTypeSet.of_list

let find_args e =
  statement_map
    (fun ?function_name ->
      let _ = function_name in
      function
      | FuncCall (name, args) ->
        print_endline @@ "args: " ^ name;
          Parse.StringMap.bindings args
          |> List.map (fun (arg, _) ->
              (name, arg))
      | _ -> [])
    e
  |> ArgSet.of_list |> ArgSet.to_list

type inferred_types = {
  variable_types : (string * InferredTypeSet.t) list;
  argument_types : ((string * string) * InferredTypeSet.t) list;
}

let infer_types_for_default_values program =
  let locals =
    List.map
      (fun sprite ->
        Parse.StringMap.bindings sprite.Untyped_ast.variables
        |> List.map (fun (name, variable) -> (name, variable)))
      program.Untyped_ast.sprites
    |> List.flatten
  in
  let globals = Parse.StringMap.bindings program.globals in
  let variables = locals @ globals in
  {
    variable_types =
      List.map
        (function
          | k, Scratch_value.Primitive v ->
              ( k,
                InferredTypeSet.of_list
                  [ Type (Scratch_value.get_primitive_type v) ] )
          | k, Scratch_value.List xs ->
              ( k,
                InferredTypeSet.singleton
                  (Type (Scratch_value.get_primitive_list_type xs)) ))
        variables;
    argument_types = [];
  }

let infer_types_for_expr e =
  {
    variable_types = List.map (fun k -> (k, infer_var_type k e)) @@ find_vars e;
    argument_types =
      List.map (fun (arg_fn, arg_name) ->
          ((arg_fn, arg_name), infer_arg_type arg_fn arg_name e))
      @@ find_args e;
  }

let merge_set_assoc_list d1 d2 =
  List.map
    (fun (k, v) ->
      ( k,
        InferredTypeSet.union
          (match Assoc_list.search k d2 with
          | Some s -> s
          | None -> InferredTypeSet.empty)
          v ))
    d1
  @ List.filter (fun (k, _) -> not @@ List.exists (fun (k', _) -> k = k') d1) d2

let merge_inferred_types t1 t2 =
  {
    variable_types = merge_set_assoc_list t1.variable_types t2.variable_types;
    argument_types = merge_set_assoc_list t1.argument_types t2.argument_types;
  }

(*let merge_inferred_types_list = List.fold_left merge_inferred_types {
      variable_types = [];
      argument_types = [];
  }*)

let infer_types program =
  merge_inferred_types
    (infer_types_for_default_values program)
    (infer_types_for_expr program.Untyped_ast.sprites)

let unify_types (k, types) cons =
  List.map (fun (k', other_types) ->
      ( k',
        if InferredTypeSet.mem (cons k) other_types then
          let other_types = InferredTypeSet.remove (cons k) other_types in
          InferredTypeSet.union types other_types
        else other_types ))

let unify_group selector cons types =
  let rec loop types = function
    | (k, _) :: ks ->
        let var_type =
          match Assoc_list.search k @@ selector types with
          | Some t -> t
          | None -> failwith "lost list element"
        in
        loop
          {
            variable_types = unify_types (k, var_type) cons types.variable_types;
            argument_types = unify_types (k, var_type) cons types.argument_types;
          }
          ks
    | [] -> types
  in
  loop types @@ selector types

let unify_variables =
  unify_group (fun x -> x.variable_types) (fun x -> Variable x)

let unify_arguments =
  unify_group
    (fun x -> x.argument_types)
    (fun (fname, argname) -> Argument (fname, argname))

let show_argument_types args =
  let args =
    List.map
      (fun ((k1, k2), v) ->
        "(" ^ k1 ^ "," ^ k2 ^ ")" ^ ": " ^ show_inferred_type_set v)
      args
    |> String.concat ", "
  in
  "[" ^ args ^ "]"

let show_variable_types vars =
  let vars =
    List.map (fun (k, v) -> k ^ ": " ^ show_inferred_type_set v) vars
    |> String.concat ", "
  in
  "[" ^ vars ^ "]"

let types program =
  let result = program |> infer_types |> unify_variables |> unify_arguments in
  print_endline @@ "arg types: " ^ show_argument_types result.argument_types;
  print_endline @@ "var types: " ^ show_variable_types result.variable_types;
  result

let reduce_to_single_type t =
  if InferredTypeSet.mem (Type String) t then Scratch_type.String
  else if InferredTypeSet.mem (Type Float) t then Float
  else Boolean

let cast t e =
  let t' = get_type e in
  if t' = t then e else Cast (e, t)

let var_type types var =
  match Assoc_list.search var types.variable_types with
  | Some t -> reduce_to_single_type t
  | None -> failwith @@ "no type inferred for: " ^ var

let arg_type types (f, a) =
  print_endline @@ "searching for: " ^ a ^ " in " ^ f;
  match Assoc_list.search (f, a) types.argument_types with
  | Some t -> reduce_to_single_type t
  | None -> failwith @@ "no type inferred for: " ^ a

let rec convert_expr ?funname types e =
  let convert = convert_expr ?funname types in
  let var_type = var_type types in
  let arg_type = arg_type types in
  match e with
  | Untyped_ast.Argument arg ->
      let funname =
        match funname with
        | Some f -> f
        | None -> failwith "arg outside function"
      in
      Typed_ast.Argument (arg, Primitive (arg_type (funname, arg)))
  | Variable var -> Variable (var, Primitive (var_type var))
  | Literal v -> Literal v
  | BinaryOperator (op, e1, e2) ->
      let t1, t2 = bin_op_input_type ?funname types e1 e2 op in
      BinaryOperator (op, convert e1 |> cast t1, convert e2 |> cast t2)
  | Not e -> Not (convert e |> cast (Primitive Boolean))
  | Index (name, i) ->
      Index
        (name, convert i |> cast (Primitive Float), Primitive (var_type name))
  | IndexOf (name, e) ->
      IndexOf (name, convert e |> cast (Primitive (var_type name)))
  | Length l -> Length l
  | Answer -> Answer

and bin_op_input_type ?funname types e1 e2 = function
  | Untyped_ast.Gt -> (Primitive Float, Primitive Float)
  | Lt -> (Primitive Float, Primitive Float)
  | Subtract -> (Primitive Float, Primitive Float)
  | Add -> (Primitive Float, Primitive Float)
  | Equals -> (
      let e1 = convert_expr ?funname types e1 in
      let e2 = convert_expr ?funname types e2 in
      let t1 = get_type e1 in
      let t2 = get_type e2 in
      match (t1, t2) with
      | Primitive String, _ -> (Primitive String, Primitive String)
      | _, Primitive String -> (Primitive String, Primitive String)
      | Primitive Float, _ -> (Primitive Float, Primitive Float)
      | _, Primitive Float -> (Primitive Float, Primitive Float)
      | _ -> (Primitive Boolean, Primitive Boolean))
  | Or -> (Primitive Boolean, Primitive Boolean)
  | Join -> (Primitive String, Primitive String)
  | LetterOf -> (Primitive Float, Primitive String)

let rec convert_statement ?funname types stmt =
  let convert = convert_expr ?funname types in
  let convert_statement = convert_statement ?funname types in
  let var_type = var_type types in
  let arg_type = arg_type types in
  match stmt with
  | Untyped_ast.FuncCall (name, es) ->
      FuncCall
        ( name,
          Parse.StringMap.bindings es
          |> List.map (fun (k, v) ->
                 (k, convert v |> cast (Primitive (arg_type (name, k)))))
          |> Parse.StringMap.of_list )
  | Branch (cond, then_branch, else_branch) ->
      Branch
        ( convert cond |> cast (Primitive Boolean),
          List.map convert_statement then_branch,
          List.map convert_statement else_branch )
  | SetVariable (name, e) ->
      SetVariable (name, convert e |> cast (Primitive (var_type name)))
  | AddToList (name, e) ->
      AddToList (name, convert e |> cast (Primitive (var_type name)))
  | DeleteAllOfList name -> DeleteAllOfList name
  | IncrVariable (name, e) ->
      IncrVariable (name, convert e |> cast (Primitive Float))
  | SetIndex s ->
      SetIndex
        ( s.list,
          convert s.index |> cast (Primitive Float),
          convert s.value |> cast (Primitive (var_type s.list)) )
  | WhileNot (cond, body) ->
      WhileNot
        ( convert cond |> cast (Primitive Boolean),
          List.map convert_statement body )
  | Repeat (cond, body) ->
      Repeat
        (convert cond |> cast (Primitive Float), List.map convert_statement body)
  | Say message -> Say (convert message |> cast (Primitive String))
  | Ask question -> Ask (convert question |> cast (Primitive String))

let convert_variable types n v =
  let t = var_type types n in
  ( n,
    (match v with
    | Scratch_value.Primitive v ->
        Scratch_value.Primitive
          (Scratch_value.cast t (Scratch_value.Primitive v))
    | Scratch_value.List l ->
        Scratch_value.List
          (List.map (fun x -> Scratch_value.Primitive x) l
          |> List.map (Scratch_value.cast t))),
    t )

let convert_function types n f =
  ( n,
    {
      parameters =
        List.map
          (fun p -> (p, Scratch_type.Primitive (arg_type types (n, p))))
          f.Untyped_ast.parameters;
      code = List.map (convert_statement ~funname:n types) f.code;
    } )

let convert_sprite types sprite =
  {
    variables =
      Parse.StringMap.mapi (convert_variable types) sprite.Untyped_ast.variables
      |> Parse.StringMap.map (fun (_, v, t) -> (v, t));
    functions =
      Parse.StringMap.mapi (convert_function types) sprite.Untyped_ast.functions
      |> Parse.StringMap.map snd;
    entry_points =
      List.map
        (List.map (convert_statement types))
        sprite.Untyped_ast.entry_points;
  }

let convert program =
  let types = types program in
  {
    globals =
      Parse.StringMap.mapi (convert_variable types) program.globals
      |> Parse.StringMap.map (fun (_, v, t) -> (v, t));
    sprites = List.map (convert_sprite types) program.Untyped_ast.sprites;
  }
