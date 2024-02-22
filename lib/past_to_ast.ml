open Format
open Pp
open Assoc_list

module type Extractor = sig
    type t
    val convert: string * Past.value -> t
    val key: string
end

let search_dict_opt k = function
    | Past.Object(_, items) -> search_assoc_list k items
    | _ -> failwith "Can only search dictionary"

let search_dict k obj = match search_dict_opt k obj with
    | Some v -> v
    | None -> failwith "item not found"

module Extractor (Ex: Extractor) = struct
    let get_targets past = match search_dict "targets" past with
        | Past.Array (_, v) -> v
        | _ -> failwith "targets have wrong types"
    let flat_map f l = List.map f l |> List.flatten
    let extract_uncoverted = flat_map (fun target -> match search_dict Ex.key target with
        | Past.Object (_, vars) -> vars
        | _ -> failwith "Wrong format")
    let extract t = get_targets t |> extract_uncoverted |> List.map Ex.convert
end

type variable = {
    id: string;
    name: string;
    value: float;
}

let pp_variable ppf { id; name; value } = fprintf ppf "variable: [id: %s name: %s value: %s]" id name (string_of_float value)
let print_variable = print_pp pp_variable


module VariableExtractor = Extractor(struct
    type t = variable
    let convert = function
        | (id, Past.Array (_, [Past.String (_, name); Past.Number (_, value)])) ->
            {id = id; name = name; value = value}
        | _ -> failwith "invalid variable format"
    let key = "variables"
end)

type input =
    | Id of string
    | Variable of string
    | Number of float
    (*| String of string*)

let pp_input ppf = function
    | Id str -> fprintf ppf "Id: %s" str
    | Variable str -> fprintf ppf "Variable: %s" str
    | Number n -> fprintf ppf "Number: %s" (string_of_float n)

type general_block = {
    id: string;
    opcode: string;
    next: string option;
    inputs: (string * input) list;
    proccode: string option;
    variable: string option;
    value: string option;
}

let pp_general_block ppf { id; opcode; next; inputs; proccode; variable; value } =
    fprintf ppf "general_block: [id: %s opcode: %s next: (%a) inputs: (%a) proccode: (%a) variable: (%a) value: (%a)]" id opcode (pp_opt pp_string) next (pp_list (fun ppf (name, input) -> fprintf ppf "%s: (%a)" name pp_input input)) inputs (pp_opt pp_string) proccode (pp_opt pp_string) variable (pp_opt pp_string) value

let print_general_block = print_pp pp_general_block

module BlockExtractor = Extractor(struct
    type t = general_block
    let convert = function
        | (id, obj) ->
                let opcode = match search_dict "opcode" obj with
                | Past.String (_, code) -> code
                | _ -> failwith "opcode has wrong data type"
                in let next = match search_dict "next" obj with
                | Past.String (_, next) -> Some next
                | Past.Null _ -> None
                | _ -> failwith "next has wrong data type"
                in let inputs = List.map (function
                    | (name, Past.Array(_, _::(Past.Array (_, [Past.Number (_, 4.); Past.String (_, n)]))::_)) -> (name, Number (float_of_string n))
                    | (name, Past.Array(_, _::(Past.Array (_, [Past.Number (_, 10.); Past.String (_, str)]))::_)) -> (name, Number (float_of_string str))
                    | (name, Past.Array(_, _::Past.String(_, id)::_)) -> (name, Id id)
                    | (name, Past.Array(_, _::(Past.Array (_, [Past.Number (_, 12.); _; Past.String (_, id)]))::_)) -> (name, Variable id)
                    | (_, value) -> Past.print_value value; failwith "Unsupported input"
                ) @@ match search_dict "inputs" obj with
                    | Past.Object (_, inputs) -> inputs
                    | _ -> failwith "inputs has wrong data type"
                in let proccode = match search_dict_opt "mutation" obj with
                    | Some mutation -> (match search_dict_opt "proccode" mutation with
                        | Some Past.String (_, str) -> Some str
                        | None -> None
                        | _ -> failwith "invalid proccode type")
                    | None -> None
                in let variable = match search_dict_opt "fields" obj with
                    | Some fields -> (match search_dict_opt "VARIABLE" fields with
                        | Some (Past.Array (_, _::(Past.String (_, var))::_)) -> Some var
                        | _ -> None)
                    | None -> None
                in let value = match search_dict_opt "fields" obj with
                    | Some fields -> (match search_dict_opt "VALUE" fields with
                        | Some (Past.Array (_, (Past.String (_, ident))::_)) -> Some ident
                        | _ -> None)
                    | None -> None
                in { id = id; opcode = opcode; next = next; inputs = inputs; proccode = proccode; variable = variable; value = value}
    let key = "blocks"
end)

type operator =
    | Gt
    | Subtract
    | Add

let pp_operator ppf = function
    | Gt -> fprintf ppf "Gt"
    | Subtract -> fprintf ppf "Subtract"
    | Add -> fprintf ppf "Add"

type block =
    | Argument of {
        id: string;
        name: string;
    }
    | ProceduresDefinition of {
        id: string;
        next: string option;
        prototype: string;
    }
    | ProceduresPrototype of {
        id: string;
        parameters: (string * string) list;
        proccode: string;
    }
    | Operator of {
        id: string;
        operator: operator;
        num1: input;
        num2: input;
    }
    | ProceduresCall of {
        id: string;
        next: string option;
        inputs: (string * input) list;
        proccode: string
    }
    | Start of {
        id: string;
        next: string option;
    }
    | IfThenElse of {
        id: string;
        next: string option;
        condition: string;
        then_branch: string option;
        else_branch: string option;
    }
    | SetVariable of {
        id: string;
        next: string option;
        variable: string;
        value: input;
    }

let pp_block ppf = function
    | Argument arg -> fprintf ppf "Argument: [id: %s name: %s]" arg.id arg.name
    | ProceduresDefinition def -> fprintf ppf "ProceduresDefinition: [id: %s next: (%a) prototype: %s" def.id (pp_opt pp_string) def.next def.prototype
    | ProceduresPrototype prot -> fprintf ppf "ProceduresPrototype: [id: %s parameters: (%a) proccode: %s]" prot.id (pp_list (fun ppf (k, v) -> fprintf ppf "%s: %s" k v)) prot.parameters prot.proccode
    | Operator op -> fprintf ppf "Operator: [id: %s operator: (%a) num1: (%a) num2: (%a)" op.id pp_operator op.operator pp_input op.num1 pp_input op.num2
    | ProceduresCall call -> fprintf ppf "ProceduresCall [id: %s next: (%a) inputs: (%a) proccode: %s]" call.id (pp_opt pp_string) call.next (pp_list (fun ppf (k, v) -> fprintf ppf "%s: %a" k pp_input v)) call.inputs call.proccode
    | Start start -> fprintf ppf "Start: [id: %s next: (%a)]" start.id (pp_opt pp_string) start.next
    | IfThenElse branch -> fprintf ppf "IfThenElse: [id: %s next: (%a) condition: %s then_branch: (%a) else_branch: (%a)" branch.id (pp_opt pp_string) branch.next branch.condition (pp_opt pp_string) branch.then_branch (pp_opt pp_string) branch.else_branch
    | SetVariable set -> fprintf ppf "SetVariable: [id: %s next: (%a) variable: %s value: (%a)]" set.id (pp_opt pp_string) set.next set.variable pp_input set.value

let pp_blocks = pp_list pp_block
let print_blocks = print_pp pp_blocks

let general_block_to_block {id; opcode; next; inputs; proccode; variable; value} =
    match opcode with
        | "procedures_definition" -> ProceduresDefinition {
            id = id;
            next = next;
            prototype = (match search_assoc_list "custom_block" inputs with
                | Some (Id prot) -> prot
                | _ -> failwith "invalid procedures_definition declaration")
        }
        | "procedures_prototype" -> ProceduresPrototype {
            id = id;
            parameters = List.map (function
                | (str1, Id str2) -> (str1, str2)
                | _ -> failwith "invalid parameter datatype"
            ) inputs;
            proccode = match proccode with Some(proccode) -> proccode | None -> failwith "invalid prototype";
        }
        | "argument_reporter_string_number" -> Argument {
            id = id;
            name = match value with Some name -> name | None -> failwith "invalid argument"
        }
        | "operator_gt" -> Operator {
            id = id;
            operator = Gt;
            num1 = (match search_assoc_list "OPERAND1" inputs with
                | Some num1 -> num1
                | _ -> failwith "operator_gt missing 1st input"
            );
            num2 = (match search_assoc_list "OPERAND2" inputs with
                | Some num2 -> num2
                | _ -> failwith "operator_gt missing 2nd input"
            )
        }
        | "operator_subtract" -> Operator {
            id = id;
            operator = Subtract;
            num1 = (match search_assoc_list "NUM1" inputs with
                | Some num1 -> num1
                | _ -> failwith "operator_subtract missing 1st input"
            );
            num2 = (match search_assoc_list "NUM2" inputs with
                | Some num2 -> num2
                | _ -> failwith "operator_subtract missing 2nd input"
            )
        }
        | "operator_add" -> Operator {
            id = id;
            operator = Add;
            num1 = (match search_assoc_list "NUM1" inputs with
                | Some num1 -> num1
                | _ -> failwith "operator_subtract missing 1st input"
            );
            num2 = (match search_assoc_list "NUM2" inputs with
                | Some num2 -> num2
                | _ -> failwith "operator_subtract missing 2nd input"
            )
        }
        | "procedures_call" -> ProceduresCall {
            id = id;
            next = next;
            inputs = inputs;
            proccode = match proccode with Some proccode -> proccode | None -> failwith "call missing proccode";
        }
        | "event_whenflagclicked" -> Start {
            id = id;
            next = next;
        }
        | "control_if_else" -> IfThenElse {
            id = id;
            next = next;
            condition = (match search_assoc_list "CONDITION" inputs with Some (Id cond) -> cond | _ -> failwith "invalid condition");
            then_branch = (match search_assoc_list "SUBSTACK" inputs with Some (Id branch) -> Some branch | _ -> None); 
            else_branch = (match search_assoc_list "SUBSTACK2" inputs with Some (Id branch) -> Some branch | _ -> None); 
        }
        | "data_setvariableto" -> SetVariable {
            id = id;
            next = next;
            variable = (match variable with Some variable -> variable | None -> failwith "missing variable");
            value = (match search_assoc_list "VALUE" inputs with Some value -> value | None -> failwith "missing value");
        }
        | _ -> failwith "invalid opcode"

let rec find_block k = function
    | (ProceduresPrototype prot)::_ when prot.id = k -> ProceduresPrototype prot
    | (ProceduresDefinition def)::_ when def.id = k -> ProceduresDefinition def
    | (Argument arg)::_ when arg.id = k -> Argument arg
    | (Operator op)::_ when op.id = k -> Operator op
    | (ProceduresCall proc)::_ when proc.id = k -> ProceduresCall proc
    | (Start s)::_ when s.id = k -> Start s
    | (IfThenElse branch)::_ when branch.id = k -> IfThenElse branch
    | (SetVariable set)::_ when set.id = k -> SetVariable set
    | _::xs -> find_block k xs
    | [] -> failwith "could not find prototype"

let rec get_arg_name_by_arg_id id = function
    | (Argument arg)::_ when arg.id = id -> arg.name
    | _::xs -> get_arg_name_by_arg_id id xs
    | [] -> failwith "could not find argument by argument id"

let get_arg_name_by_input_id id blocks =
    let rec loop = function
        | (ProceduresPrototype prot)::xs -> (match search_assoc_list id prot.parameters with
            | Some id -> get_arg_name_by_arg_id id blocks
            | None -> loop xs)
        | _::xs -> loop xs
        | [] -> failwith "could not find argument by input id"
    in loop blocks

let rec get_variable_name_by_id id = function
    | (var: variable)::_ when var.id = id -> var.name
    | _::vars -> get_variable_name_by_id id vars
    | [] -> failwith "could not find variable"

let rec create_number_expr_from_input blocks variables = function
    | Id id -> create_number_expr_from_block blocks variables @@ find_block id blocks
    | Number n -> Ast.Literal n
    | Variable v -> Ast.Variable (get_variable_name_by_id v variables)
    (*| String str -> failwith @@ "strings are currently not supported: " ^ str*)
and create_number_expr_from_block blocks variables = function
    | Argument arg -> Ast.Argument arg.name
    | Operator op when op.operator = Subtract -> Ast.Subtract (create_number_expr_from_input blocks variables op.num1, create_number_expr_from_input blocks variables op.num2)
    | Operator op when op.operator = Add -> Ast.Add (create_number_expr_from_input blocks variables op.num1, create_number_expr_from_input blocks variables op.num2)
    | _ -> failwith "unsupported expression"

and create_bool_expr_from_block blocks variables id = match find_block id blocks with
    | Operator op when op.operator = Gt -> Ast.GreaterThan (create_number_expr_from_input blocks variables op.num1, create_number_expr_from_input blocks variables op.num2)
    | _ -> failwith "unsupported bool expression"

let rec create_statements blocks variables = function
    | Some id -> (match find_block id blocks with
        | ProceduresCall call -> Ast.FuncCall (call.proccode, List.map (fun (str, input) -> (get_arg_name_by_input_id str blocks, create_number_expr_from_input blocks variables input)) call.inputs) :: (create_statements blocks variables call.next)
        | IfThenElse branch -> (Ast.Branch (create_bool_expr_from_block blocks variables branch.condition, create_statements blocks variables branch.then_branch, create_statements blocks variables branch.else_branch)) :: (create_statements blocks variables branch.next)
        | SetVariable set -> (Ast.SetVariable (get_variable_name_by_id set.variable variables, create_number_expr_from_input blocks variables set.value ) :: (create_statements blocks variables set.next))
        | _ -> failwith "invalid statement"
        )
    | None -> []
let create_functions blocks variables =
    let rec loop blocks = function
        | (ProceduresDefinition def)::xs ->
                (match find_block def.prototype blocks with
                    | ProceduresPrototype prot -> (prot.proccode, Ast.({
                        parameters = List.map (fun (_, id) -> get_arg_name_by_arg_id id blocks) prot.parameters;
                        statements = create_statements blocks variables def.next;
                    }))::(loop blocks xs)
                    | _ -> failwith "invalid prototype type")
        | _::xs -> loop blocks xs
        | [] -> []
    in loop blocks blocks

let create_main blocks variables =
    let rec loop blocks = function
        | (Start s)::_ -> create_statements blocks variables s.next
        | _::xs -> loop blocks xs
        | [] -> failwith "program is missing a start block"
    in loop blocks blocks

let create_program blocks variables = Ast.({
        functions = create_functions blocks variables;
        variables = List.map (fun (var: variable) -> (var.name, var.value)) variables;
        main = create_main blocks variables;
    })

let translate_past past =
    let vars = VariableExtractor.extract past
    in let blocks = BlockExtractor.extract past |> List.map general_block_to_block
    in create_program blocks vars
