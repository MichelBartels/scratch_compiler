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
    let declare name scratch_function =
        let param_types = List.map (fun (_, scratch_type) ->
            Scratch_type.to_lltype scratch_type context
        ) scratch_function.Typed_ast.parameters in
        let ty = Llvm.function_type (Llvm.void_type context) (Array.of_list param_types) in
        let f = Llvm.declare_function name ty llmodule in
        let parameters = List.map2 (fun (name, _) param ->
            (name, param)
        ) scratch_function.parameters (Array.to_list (Llvm.params f)) in
        let entry = Llvm.append_block context "" f in
        { parameters; f; ty; entry }
    let param name f =
        List.assoc name f.parameters
    let call f args =
        let args = List.map (fun (k, _) -> List.assoc k args) f.parameters in
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

let create_literal = function
    | Scratch_value.Primitive (Float n) -> Llvm.const_float (Llvm.double_type context) n
    | Primitive (Boolean b) -> Llvm.const_int (Llvm.i1_type context) (if b then 1 else 0)
    | Primitive (String s) ->
        let str = Llvm.build_global_stringptr s "" builder in
        RuntimeFunction.call alloc_string [str]
    | List _ -> failwith "Lists have a separate scope"

let init_variable name var =
    let scratch_type = Scratch_value.get_type var in
    let lltype = Scratch_type.to_lltype scratch_type context in
    let global = Llvm.declare_global lltype name llmodule in
    Llvm.set_initializer (create_literal var) global;
    (lltype, global)

let init_answer () =
    let lltype = Llvm.pointer_type context in
    let answer = Llvm.declare_global lltype "answer" llmodule in
    Llvm.set_initializer (Llvm.const_pointer_null lltype) answer;
    answer

let init_variables vars =
    List.map (fun (name, var) -> (name, init_variable name var)) vars

let rec convert_expr cur_fn vars funcs answer e =
    let convert_expr = convert_expr cur_fn vars funcs answer in (match e with
    | Argument (name, _) -> Function.param name cur_fn
    | Variable (name, _) -> let (lltype, var) = List.assoc name vars in Llvm.build_load lltype var "" builder
    | Literal lit -> create_literal lit
    | BinaryOperator (op, e1, e2) ->
        let e1 = convert_expr e1 in
        let e2 = convert_expr e2 in
        let op = match op with
            | Gt -> Llvm.build_fcmp Llvm.Fcmp.Ogt
            | Lt -> Llvm.build_fcmp Llvm.Fcmp.Olt
            | Subtract -> Llvm.build_fsub
            | Add -> Llvm.build_fadd
            | Equals -> Llvm.build_fcmp Llvm.Fcmp.Oeq
            | Or -> Llvm.build_or
            | Join -> failwith "Join is not implemented"
            | LetterOf -> failwith "LetterOf is not implemented"
        in
        op e1 e2 "" builder
    | Not e ->
        let e = convert_expr e in
        Llvm.build_not e "" builder
    | FuncCall (name, args) ->
        let args = List.map (fun (k, v) -> (k, convert_expr v)) args in
        Function.call (List.assoc name funcs) args
    | Branch (cond, then_branch, else_branch) ->
        let cond = convert_expr cond in
        let then_block = Llvm.append_block context "" cur_fn.f in
        let else_block = Llvm.append_block context "" cur_fn.f in
        let next_block = Llvm.append_block context "" cur_fn.f in
        let branch = Llvm.build_cond_br cond then_block else_block builder in
        Llvm.position_at_end then_block builder; 
        ignore @@ List.map convert_expr then_branch;
        ignore @@ Llvm.build_br next_block builder;
        Llvm.position_at_end else_block builder;
        ignore @@ List.map convert_expr else_branch;
        ignore @@ Llvm.build_br next_block builder;
        Llvm.position_at_end next_block builder;
        branch
    | SetVariable (var, e) ->
        let value = convert_expr e in
        let (_, var) = List.assoc var vars in
        Llvm.build_store value var builder
    | Say e -> RuntimeFunction.call say [convert_expr e]
    | Cast (e, to_type) -> (let from_type = Typed_ast.get_type e in match (from_type, to_type) with
        | (Some Primitive Float, Primitive String) -> RuntimeFunction.call cast_double_to_string [convert_expr e]
        | (Some Primitive String, Primitive Float) -> RuntimeFunction.call cast_string_to_double [convert_expr e]
        | _ -> failwith "Unsupported cast"
    )
    | Ask e -> let question = convert_expr e in RuntimeFunction.call ask [question; answer]
    | Answer -> answer
    | _ -> failwith "Unsupported expression"
    )

let convert_function scratch_f f =
    Llvm.position_at_end f builder;
    convert_expr scratch_f

type program = {
    vars: (string * (Llvm.lltype * Llvm.llvalue)) list;
    functions: (string * Function.t) list;
}

let convert (p: Typed_ast.program) =
    let vars = init_variables p.variables in
    let answer = init_answer () in
    let functions = List.map (fun (k, f) -> (k, Function.declare k f)) p.functions in
    ignore @@ List.map2 (fun (_, scratch_f) (_, f) ->
        Llvm.position_at_end f.Function.entry builder;
        ignore @@ List.map (convert_expr f vars functions answer) scratch_f.statements;
        Llvm.build_ret_void builder
    ) p.functions functions;
    let main = Llvm.declare_function "main" (Llvm.function_type (Llvm.void_type context) [||]) llmodule in
    let main_entry = Llvm.append_block context "" main in
    Llvm.position_at_end main_entry builder;
    ignore @@ List.map (convert_expr { parameters = []; f = main; entry = main_entry; ty = Llvm.void_type context } vars functions answer) p.main;
    ignore @@ Llvm.build_ret_void builder;
    { vars; functions }

let aot_compile () =
    Llvm_analysis.assert_valid_module llmodule;
    Llvm_all_backends.initialize ();
    let target_triple = Llvm_target.Target.default_triple () in
    let target = Llvm_target.Target.by_triple target_triple in
    let target_machine = Llvm_target.TargetMachine.create ~triple:target_triple target in
    let target_data = Llvm_target.TargetMachine.data_layout target_machine |> Llvm_target.DataLayout.as_string in
    Llvm.set_data_layout target_data llmodule;
    Llvm.set_target_triple target_triple llmodule;
    let pass_manager = Llvm.PassManager.create () in
    Llvm_target.TargetMachine.add_analysis_passes pass_manager target_machine;
    Llvm.PassManager.run_module llmodule pass_manager |> ignore;
    (*Llvm_passmgr_builder.set_opt_level 3 pass_manager;*)
    Llvm_target.TargetMachine.emit_to_file llmodule Llvm_target.CodeGenFileType.ObjectFile "out.o" target_machine;
    Llvm.PassManager.dispose pass_manager;
    Llvm.dispose_module llmodule;
    Llvm.dispose_context context
