open Typed_ast

let context = Llvm.create_context ()
let llmodule = Llvm.create_module context "module"
let builder = Llvm.builder context


module Function = struct
    type t = {
        parameters: (string * Llvm.llvalue) list;
        f: Llvm.llvalue;
        entry: Llvm.llbasicblock;
        ty: Llvm.lltype;
    }
    let declare scratch_function =
        let param_types = List.map (fun (_, scratch_type) ->
            Scratch_type.to_lltype scratch_type context
        ) scratch_function.Typed_ast.parameters in
        let ty = Llvm.function_type (Llvm.void_type context) (Array.of_list param_types) in
        let f = Llvm.declare_function "" ty llmodule in
        let parameters = List.map2 (fun (name, _) param ->
            (name, param)
        ) scratch_function.parameters (Array.to_list (Llvm.params f)) in
        let entry = Llvm.append_block context "" f in
        { parameters; f; ty; entry }
    let param name f =
        List.assoc name f.parameters
    let call f args =
        let args = List.map (fun (k, _) -> Parse.StringMap.find k args) f.parameters in
        Llvm.build_call f.ty f.f (Array.of_list args) "" builder
end

module RuntimeFunction = struct
    type t = {
        f: Llvm.llvalue;
        ty: Llvm.lltype;
    }
    let declare name input_types output_type =
        let input_types = Array.of_list input_types in
        let ty = Llvm.function_type output_type input_types in
        let f = Llvm.declare_function name ty llmodule in
        { f; ty }
    let call f args =
        Llvm.build_call f.ty f.f (Array.of_list args) "" builder
end

let alloc_string = RuntimeFunction.declare "alloc_string" [Llvm.pointer_type context] (Llvm.pointer_type context)
(*let alloc_empty_vec = RuntimeFunction.declare "alloc_empty_vec" [] (Llvm.pointer_type context)*)

let say = RuntimeFunction.declare "say" [Llvm.pointer_type context] (Llvm.void_type context)
let ask = RuntimeFunction.declare "ask" [Llvm.pointer_type context; Llvm.pointer_type context] (Llvm.void_type context)

let cast_double_to_string = RuntimeFunction.declare "cast_f64_to_string" [Llvm.double_type context] (Llvm.pointer_type context)
let cast_string_to_double = RuntimeFunction.declare "cast_string_to_f64" [Llvm.pointer_type context] (Llvm.double_type context)

let join = RuntimeFunction.declare "join" [Llvm.pointer_type context; Llvm.pointer_type context] (Llvm.pointer_type context)
let letter_of = RuntimeFunction.declare "letter_of" [Llvm.pointer_type context; Llvm.double_type context] (Llvm.pointer_type context)
let string_eq = RuntimeFunction.declare "string_eq" [Llvm.pointer_type context; Llvm.pointer_type context] (Llvm.i1_type context)

let push_to_string_vec = RuntimeFunction.declare "push_to_string_vec" [Llvm.pointer_type context; Llvm.pointer_type context] (Llvm.void_type context)
let push_to_f64_vec = RuntimeFunction.declare "push_to_f64_vec" [Llvm.pointer_type context; Llvm.double_type context] (Llvm.void_type context)
let push_to_bool_vec = RuntimeFunction.declare "push_to_bool_vec" [Llvm.pointer_type context; Llvm.i1_type context] (Llvm.void_type context)

let alloc_empty_string_vec = RuntimeFunction.declare "alloc_empty_string_vec" [] (Llvm.pointer_type context)
let alloc_empty_f64_vec = RuntimeFunction.declare "alloc_empty_f64_vec" [] (Llvm.pointer_type context)
let alloc_empty_bool_vec = RuntimeFunction.declare "alloc_empty_bool_vec" [] (Llvm.pointer_type context)

let clear_string_vec = RuntimeFunction.declare "clear_string_vec" [Llvm.pointer_type context] (Llvm.void_type context)
let clear_f64_vec = RuntimeFunction.declare "clear_f64_vec" [Llvm.pointer_type context] (Llvm.void_type context)
let clear_bool_vec = RuntimeFunction.declare "clear_bool_vec" [Llvm.pointer_type context] (Llvm.void_type context)

let get_string_vec_element = RuntimeFunction.declare "get_string_vec_element" [Llvm.pointer_type context; Llvm.double_type context] (Llvm.pointer_type context)
let get_f64_vec_element = RuntimeFunction.declare "get_f64_vec_element" [Llvm.pointer_type context; Llvm.double_type context] (Llvm.double_type context)
let get_bool_vec_element = RuntimeFunction.declare "get_bool_vec_element" [Llvm.pointer_type context; Llvm.double_type context] (Llvm.i1_type context)

let index_of_string = RuntimeFunction.declare "index_of_string" [Llvm.pointer_type context; Llvm.pointer_type context] (Llvm.double_type context)
let index_of_f64 = RuntimeFunction.declare "index_of_f64" [Llvm.pointer_type context; Llvm.double_type context] (Llvm.double_type context)
let index_of_bool = RuntimeFunction.declare "index_of_bool" [Llvm.pointer_type context; Llvm.i1_type context] (Llvm.double_type context)

let set_string_vec_element = RuntimeFunction.declare "set_string_vec_element" [Llvm.pointer_type context; Llvm.double_type context; Llvm.pointer_type context] (Llvm.void_type context)
let set_f64_vec_element = RuntimeFunction.declare "set_f64_vec_element" [Llvm.pointer_type context; Llvm.double_type context; Llvm.double_type context] (Llvm.void_type context)
let set_bool_vec_element = RuntimeFunction.declare "set_bool_vec_element" [Llvm.pointer_type context; Llvm.double_type context; Llvm.i1_type context] (Llvm.void_type context)

let len_of_string_vec = RuntimeFunction.declare "len_of_string_vec" [Llvm.pointer_type context] (Llvm.double_type context)
let len_of_f64_vec = RuntimeFunction.declare "len_of_f64_vec" [Llvm.pointer_type context] (Llvm.double_type context)
let len_of_bool_vec = RuntimeFunction.declare "len_of_bool_vec" [Llvm.pointer_type context] (Llvm.double_type context)

let create_literal = function
    | Scratch_value.Float n -> Llvm.const_float (Llvm.double_type context) n
    | Boolean b -> Llvm.const_int (Llvm.i1_type context) (if b then 1 else 0)
    | String s ->
        let str = Llvm.build_global_stringptr s "" builder in
        RuntimeFunction.call alloc_string [str]

let init_primitive var scratch_type =
    let lltype = Scratch_type.to_lltype (Scratch_type.Primitive scratch_type) context in
    let global = Llvm.declare_global lltype "" llmodule in
    let literal = create_literal var in
    Llvm.set_initializer (Llvm.const_null lltype) global;
    ignore @@ Llvm.build_store literal global builder;
    global

let init_list value type_ =
    let list = Llvm.declare_global (Llvm.pointer_type context) "" llmodule in
    let constructor = match type_ with
    | Scratch_type.Float -> alloc_empty_f64_vec
    | String -> alloc_empty_string_vec
    | Boolean -> alloc_empty_bool_vec in
    let list_value = RuntimeFunction.call constructor [] in
    Llvm.set_initializer (Llvm.const_null (Llvm.pointer_type context)) list;
    ignore @@ Llvm.build_store list_value list builder;
    List.iter (fun v -> ignore @@ match type_ with
        | Float -> RuntimeFunction.call push_to_f64_vec [list_value; create_literal v]
        | String -> RuntimeFunction.call push_to_string_vec [list_value; create_literal v]
        | Boolean -> RuntimeFunction.call push_to_bool_vec [list_value; create_literal v]
    ) value;
    list

let init_variable (value, scratch_type) = (scratch_type, match value with
    | Scratch_value.Primitive v -> init_primitive v scratch_type
    | Scratch_value.List l -> init_list l scratch_type)

let init_answer () =
    let lltype = Llvm.pointer_type context in
    let answer = Llvm.declare_global lltype "answer" llmodule in
    Llvm.set_initializer (Llvm.const_pointer_null lltype) answer;
    answer

let init_variables = Parse.StringMap.map init_variable

let assert_primitive = function
    | Scratch_value.Primitive v -> v
    | _ -> failwith "Unsupported type"

let rec convert_expr cur_fn vars funcs answer e =
    let convert_expr = convert_expr cur_fn vars funcs answer in (match e with
    | Argument (name, _) -> Function.param name cur_fn
    | Variable (name, _) -> let (scratch_type, var) = Parse.StringMap.find name vars in Llvm.build_load (Scratch_type.to_lltype (Scratch_type.Primitive scratch_type) context) var "" builder
    | Literal lit -> assert_primitive lit |> create_literal
    | BinaryOperator (op, e1, e2) ->
        let e1' = convert_expr e1 in
        let e2' = convert_expr e2 in
        let op = match op with
            | Gt -> Llvm.build_fcmp Llvm.Fcmp.Ogt
            | Lt -> Llvm.build_fcmp Llvm.Fcmp.Olt
            | Subtract -> Llvm.build_fsub
            | Add -> Llvm.build_fadd
            | Equals -> (match Typed_ast.get_type e1 with
                | Primitive Float -> Llvm.build_fcmp Llvm.Fcmp.Oeq
                | Primitive String -> (fun e1 e2 _ _ -> RuntimeFunction.call string_eq [e1; e2])
                | Primitive Boolean -> Llvm.build_icmp Llvm.Icmp.Eq
                | _ -> failwith "Unsupported type for equals")
            | Or -> Llvm.build_or
            | Join -> (fun e1 e2 _ _ -> RuntimeFunction.call join [e1; e2])
            | LetterOf -> (fun e1 e2 _ _ -> RuntimeFunction.call letter_of [e2; e1])
        in
        op e1' e2' "" builder
    | Not e ->
        let e = convert_expr e in
        Llvm.build_not e "" builder
    | Index (l, e, _) ->
        let (list_type, list) = Parse.StringMap.find l vars in
        let list = Llvm.build_load (Llvm.pointer_type context) list "" builder in
        let index = convert_expr e in
        (match list_type with
            | Scratch_type.Float -> RuntimeFunction.call get_f64_vec_element [list; index]
            | String -> RuntimeFunction.call get_string_vec_element [list; index]
            | Boolean -> RuntimeFunction.call get_bool_vec_element [list; index])
    | IndexOf (l, e) ->
        let (list_type, list) = Parse.StringMap.find l vars in
        let list = Llvm.build_load (Llvm.pointer_type context) list "" builder in
        let value = convert_expr e in
        (match list_type with
            | Scratch_type.Float -> RuntimeFunction.call index_of_f64 [list; value]
            | String -> RuntimeFunction.call index_of_string [list; value]
            | Boolean -> RuntimeFunction.call index_of_bool [list; value])
    | Length l ->
        let (list_type, list) = Parse.StringMap.find l vars in
        (match list_type with
            | Scratch_type.Float -> RuntimeFunction.call len_of_f64_vec [list]
            | String -> RuntimeFunction.call len_of_string_vec [list]
            | Boolean -> RuntimeFunction.call len_of_bool_vec [list])
    | Cast (e, to_type) -> (let from_type = Typed_ast.get_type e in match (from_type, to_type) with
        | (Primitive Float, Primitive String) -> RuntimeFunction.call cast_double_to_string [convert_expr e]
        | (Primitive String, Primitive Float) -> RuntimeFunction.call cast_string_to_double [convert_expr e]
        | _ -> failwith "Unsupported cast"
    )
    | Answer -> answer
    )

let rec convert_statement cur_fn vars funcs answer stmt =
    let convert_expr = convert_expr cur_fn vars funcs answer in
    let convert_statement = convert_statement cur_fn vars funcs answer in match stmt with
    | FuncCall (name, args) ->
        let args = Parse.StringMap.map convert_expr args in
        Function.call (Parse.StringMap.find name funcs) args
    | Branch (cond, then_branch, else_branch) ->
        let cond = convert_expr cond in
        let then_block = Llvm.append_block context "" cur_fn.f in
        let else_block = Llvm.append_block context "" cur_fn.f in
        let next_block = Llvm.append_block context "" cur_fn.f in
        let branch = Llvm.build_cond_br cond then_block else_block builder in
        Llvm.position_at_end then_block builder;
        ignore @@ List.map convert_statement then_branch;
        ignore @@ Llvm.build_br next_block builder;
        Llvm.position_at_end else_block builder;
        ignore @@ List.map convert_statement else_branch;
        ignore @@ Llvm.build_br next_block builder;
        Llvm.position_at_end next_block builder;
        branch
    | SetVariable (var, e) ->
        let value = convert_expr e in
        let (_, var) = Parse.StringMap.find var vars in
        Llvm.build_store value var builder
    | AddToList (l, e) ->
        let (list_type, list) = Parse.StringMap.find l vars in
        let list = Llvm.build_load (Llvm.pointer_type context) list "" builder in
        let value = convert_expr e in
        (match list_type with
            | Scratch_type.Float -> RuntimeFunction.call push_to_f64_vec [list; value]
            | String -> RuntimeFunction.call push_to_string_vec [list; value]
            | Boolean -> RuntimeFunction.call push_to_bool_vec [list; value])
    | DeleteAllOfList l ->
        let (list_type, list) = Parse.StringMap.find l vars in
        let list = Llvm.build_load (Llvm.pointer_type context) list "" builder in
        (match list_type with
            | Scratch_type.Float -> RuntimeFunction.call clear_f64_vec [list]
            | String -> RuntimeFunction.call clear_string_vec [list]
            | Boolean -> RuntimeFunction.call clear_bool_vec [list])
    | IncrVariable (var, e) ->
        let value = convert_expr e in
        let (_, var) = Parse.StringMap.find var vars in
        let current = Llvm.build_load (Llvm.type_of value) var "" builder in
        let new_value = Llvm.build_fadd current value "" builder in
        Llvm.build_store new_value var builder
    | SetIndex (l, i, e) ->
        let (list_type, list) = Parse.StringMap.find l vars in
        let list = Llvm.build_load (Llvm.pointer_type context) list "" builder in
        let index = convert_expr i in
        let value = convert_expr e in
        (match list_type with
            | Scratch_type.Float -> RuntimeFunction.call set_f64_vec_element [list; index; value]
            | String -> RuntimeFunction.call set_string_vec_element [list; index; value]
            | Boolean -> RuntimeFunction.call set_bool_vec_element [list; index; value])
    | WhileNot (cond, body) ->
        let cond_block = Llvm.append_block context "" cur_fn.f in
        let loop = Llvm.append_block context "" cur_fn.f in
        let next_block = Llvm.append_block context "" cur_fn.f in
        ignore @@ Llvm.build_br cond_block builder;
        Llvm.position_at_end cond_block builder;
        let cond = convert_expr (Not cond) in
        let branch = Llvm.build_cond_br cond loop next_block builder in
        Llvm.position_at_end loop builder;
        ignore @@ List.map convert_statement body;
        ignore @@ Llvm.build_br cond_block builder;
        Llvm.position_at_end next_block builder;
        branch
    | Repeat (n, body) ->
        let n = convert_expr n in
        let i = Llvm.build_alloca (Llvm.double_type context) "" builder in
        ignore @@ Llvm.build_store n i builder;
        let cond_block = Llvm.append_block context "" cur_fn.f in
        let loop = Llvm.append_block context "" cur_fn.f in
        let next_block = Llvm.append_block context "" cur_fn.f in
        ignore @@ Llvm.build_br cond_block builder;
        Llvm.position_at_end cond_block builder;
        let cur_i = Llvm.build_load (Llvm.double_type context) i "" builder in
        let cond = Llvm.build_fcmp Llvm.Fcmp.One cur_i (Llvm.const_float (Llvm.double_type context) 0.0) "" builder in
        let new_i = Llvm.build_fsub cur_i (Llvm.const_float (Llvm.double_type context) 1.0) "" builder in
        ignore @@ Llvm.build_store new_i i builder;
        let branch = Llvm.build_cond_br cond loop next_block builder in
        Llvm.position_at_end loop builder;
        ignore @@ List.map convert_statement body;
        ignore @@ Llvm.build_br cond_block builder;
        Llvm.position_at_end next_block builder;
        branch
    | Say e -> RuntimeFunction.call say [convert_expr e]
    | Ask e -> let question = convert_expr e in RuntimeFunction.call ask [question; answer]

let convert_function scratch_f f =
    Llvm.position_at_end f builder;
    convert_statement scratch_f

let convert_sprite answer globals (sprite: sprite) =
    let functions = Parse.StringMap.map Function.declare sprite.functions in
    let vars = Parse.StringMap.map init_variable sprite.variables in
    let vars = Parse.StringMap.union (fun _ -> failwith "collision") vars globals in
    ignore @@ Parse.StringMap.mapi (fun name scratch_f ->
        let f = Parse.StringMap.find name functions in
        Llvm.position_at_end f.Function.entry builder;
        ignore @@ List.map (convert_statement f vars functions answer) scratch_f.code;
        Llvm.build_ret_void builder
    ) sprite.functions;
    let entry_points = List.map (fun code ->
        let f = Function.declare { code; parameters = [] } in
        Llvm.position_at_end f.Function.entry builder;
        ignore @@ List.map (convert_statement f vars functions answer) code;
        ignore @@ Llvm.build_ret_void builder;
        f)  sprite.entry_points in
    entry_points

let convert (p: Typed_ast.program) =
    let main = Llvm.declare_function "main" (Llvm.function_type (Llvm.void_type context) [||]) llmodule in
    let entry = Llvm.append_block context "" main in
    Llvm.position_at_end entry builder;
    let answer = init_answer () in
    let globals = init_variables p.globals in
    let entry_points = List.map (fun sprite ->
        Llvm.position_at_end entry builder;
        convert_sprite answer globals sprite) p.sprites
    |> List.concat in
    Llvm.position_at_end entry builder;
    ignore @@ List.map (fun entry_point -> Function.call entry_point Parse.StringMap.empty) entry_points;
    ignore @@ Llvm.build_ret_void builder

let aot_compile () =
    Llvm_analysis.assert_valid_module llmodule;
    Llvm_all_backends.initialize ();
    let target_triple = Llvm_target.Target.default_triple () in
    let target = Llvm_target.Target.by_triple target_triple in
    let target_machine = Llvm_target.TargetMachine.create ~triple:target_triple target in
    let target_data = Llvm_target.TargetMachine.data_layout target_machine |> Llvm_target.DataLayout.as_string in
    Llvm.set_data_layout target_data llmodule;
    Llvm.set_target_triple target_triple llmodule;
    let passbuilder_options = Llvm_passbuilder.create_passbuilder_options () in
    Llvm_passbuilder.run_passes llmodule "default<O3>" target_machine passbuilder_options |> Result.get_ok;
    (*Llvm_passmgr_builder.set_opt_level 3 pass_manager;*)
    Llvm_target.TargetMachine.emit_to_file llmodule Llvm_target.CodeGenFileType.ObjectFile "out.o" target_machine;
    (*Llvm_passbuilder.dispose_passbuilder_options passbuilder_options;*)
    Llvm.dispose_module llmodule;
    Llvm.dispose_context context
