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

let alloc_string = RuntimeFunction.declare "alloc_string" [Llvm.i64_type context] (Llvm.pointer_type context)
(*let alloc_empty_vec = RuntimeFunction.declare "alloc_empty_vec" [] (Llvm.pointer_type context)*)

let create_literal = function
    | Scratch_value.NumberValue n -> Llvm.const_float (Llvm.double_type context) n
    | BoolValue b -> Llvm.const_int (Llvm.i1_type context) (if b then 1 else 0)
    | StringValue s -> Llvm.const_stringz context s
    | ListValue _ -> failwith "Lists have a separate scope"

let init_variable name var =
    let scratch_type = Scratch_value.get_type var in
    let lltype = Scratch_type.to_lltype scratch_type context in
    let global = Llvm.declare_global lltype name llmodule in
    Llvm.set_initializer (create_literal var) global;
    global

let init_variables vars =
    List.map (fun (name, var) -> (name, init_variable name var)) vars

let rec convert_expr cur_fn vars funcs e =
    let convert_expr = convert_expr cur_fn vars funcs in (match e with
    | Argument (name, _) -> Function.param name cur_fn
    | Variable (name, _) -> List.assoc name vars
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
        Llvm.position_at_end then_block builder;
        branch
    | SetVariable (var, e) ->
        let value = convert_expr e in
        let var = List.assoc var vars in
        Llvm.build_store value var builder
    | _ -> failwith "unsupported instruction"
    )

let convert_function scratch_f f =
    Llvm.position_at_end f builder;
    convert_expr scratch_f

type program = {
    vars: (string * Llvm.llvalue) list;
    functions: Function.t list;
}

let convert (p: Typed_ast.program) =
    let vars = init_variables p.variables in
    let functions = List.map (fun (k, f) -> Function.declare k f) p.functions in
    ignore @@ List.map2 (fun (_, scratch_f) f ->
        Llvm.position_at_end f.entry builder;
        ignore @@ List.map (convert_expr f vars functions) scratch_f
    ) p.functions functions;
    
