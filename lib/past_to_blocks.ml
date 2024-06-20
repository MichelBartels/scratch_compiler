open Blocks

module type Extractor = sig
    type t
    val convert: string * Past.value -> t
    val key: string
end

let search_dict_opt k = function
    | Past.Object(_, items) -> Assoc_list.search k items
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

let create_value = Scratch_value.(function
    | Past.Number (_, value) -> Float value
    | Past.String (_, value) -> (match float_of_string_opt value with
        | Some f -> Float f
        | None -> String value
    )
    | _ -> failwith "invalid value")

module VariableExtractor = Extractor(struct
    type t = variable
    let convert = function
        | (id, Past.Array (_, [Past.String (_, name); value])) -> 
                {id = id;
                name = name;
                value = Primitive (create_value value)
                }
        | _ -> failwith "invalid variable format"
    let key = "variables"
end)

module ListExtractor = Extractor(struct
    type t = variable
    let convert = function
        | (id, Past.Array (_, [Past.String(_, name); Past.Array(_, vals)])) -> {
            id = id;
            name = name;
            value = List (List.map create_value vals)
        }
        | _ -> failwith "invalid list format"
    let key = "lists"
end)

type general_block = {
    id: string;
    opcode: string;
    next: string option;
    inputs: (string * input) list;
    proccode: string option;
    fields: (string * string list) list;
    value: string option;
}

let create_value = function
    | Past.Array(_, _::(Past.Array (_, [Past.Number (_, 4.); Past.String (_, n)]))::_) -> Some (Value (Primitive (Float (float_of_string n))))
    | Past.Array(_, _::(Past.Array (_, [Past.Number (_, 10.); Past.String (_, str)]))::_) -> Some (match float_of_string_opt str with
        | Some f -> Value (Primitive (Float f))
        | None -> Value (Primitive (String str))
    )
    | Past.Array(_, _::Past.String(_, id)::_) -> Some (Id id)
    | Past.Array(_, _::(Past.Array (_, [Past.Number (_, 12.); _; Past.String (_, id)]))::_) -> Some (Variable id)
    | _ -> None
    (*| v -> failwith @@ "unsupported input" ^ Past.show_value v*)


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
                in let inputs = List.filter_map (fun (name, value) -> match create_value value with Some value -> Some (name, value) | None -> None) @@ match search_dict "inputs" obj with
                    | Past.Object (_, inputs) -> inputs
                    | _ -> failwith "inputs has wrong data type"
                in let proccode = match search_dict_opt "mutation" obj with
                    | Some mutation -> (match search_dict_opt "proccode" mutation with
                        | Some Past.String (_, str) -> Some str
                        | None -> None
                        | _ -> failwith "invalid proccode type")
                    | None -> None
                in let fields = match search_dict_opt "fields" obj with
                    | Some (Past.Object (_, fields)) -> (List.map (function
                        | (key, Past.Array (_, vals)) -> (key, List.filter_map (function
                            | Past.String (_, str) -> Some str
                            | Past.Null _ -> None
                            | _ -> failwith @@ "a field must be an array of strings or nulls: " ^ key
                        ) vals)
                        | _ -> failwith "a field must be an array"
                    ) fields)
                    | _ -> failwith "an object must have a fields entry"
                in let value = match search_dict_opt "fields" obj with
                    | Some fields -> (match search_dict_opt "VALUE" fields with
                        | Some (Past.Array (_, (Past.String (_, ident))::_)) -> Some ident
                        | _ -> None)
                    | None -> None
                in { id = id; opcode = opcode; next = next; inputs = inputs; proccode = proccode; fields = fields; value = value}
    let key = "blocks"
end)

let create_op_block id inputs op arg1 arg2 = BinaryOperator {
        id = id;
        operator = op;
        arg1 = (match Assoc_list.search arg1 inputs with
            | Some arg -> arg
            | _ -> failwith "operator missing 1st input"
        );
        arg2 = (match Assoc_list.search arg2 inputs with
            | Some arg -> arg
            | _ -> failwith "operator missing 2nd input"
        );
    }

let general_block_to_block {id; opcode; next; inputs; proccode; fields; value} =
    match opcode with
        | "procedures_definition" -> ProceduresDefinition {
            id = id;
            next = next;
            prototype = (match Assoc_list.search "custom_block" inputs with
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
        | "operator_gt" -> create_op_block id inputs Gt "OPERAND1" "OPERAND2"
        | "operator_lt" -> create_op_block id inputs Lt "OPERAND1" "OPERAND2"
        | "operator_equals" -> create_op_block id inputs Equals "OPERAND1" "OPERAND2"
        | "operator_or" -> create_op_block id inputs Or "OPERAND1" "OPERAND2"
        | "operator_subtract" -> create_op_block id inputs Subtract "NUM1" "NUM2"
        | "operator_add" -> create_op_block id inputs Add "NUM1" "NUM2"
        | "operator_join" -> create_op_block id inputs Join "STRING1" "STRING2"
        | "operator_letter_of" -> create_op_block id inputs LetterOf "LETTER" "STRING"
        | "operator_not" -> Not {
            id = id;
            arg = (match Assoc_list.search "OPERAND" inputs with Some op -> op | None -> failwith "missing arg");
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
            condition = (match Assoc_list.search "CONDITION" inputs with Some (Id cond) -> cond | Some cond -> failwith @@ "invalid condition: " ^ show_input cond | None -> failwith "missing condition");
            then_branch = (match Assoc_list.search "SUBSTACK" inputs with Some (Id branch) -> Some branch | _ -> None); 
            else_branch = (match Assoc_list.search "SUBSTACK2" inputs with Some (Id branch) -> Some branch | _ -> None); 
        }
        | "control_if" -> IfThenElse {
            id = id;
            next = next;
            condition = (match Assoc_list.search "CONDITION" inputs with Some (Id cond) -> cond | Some cond -> failwith @@ "invalid condition: " ^ show_input cond | None -> failwith "missing condition");
            then_branch = (match Assoc_list.search "SUBSTACK" inputs with Some (Id branch) -> Some branch | _ -> None); 
            else_branch = None
        }
        | "data_setvariableto" -> SetVariable {
            id = id;
            next = next;
            variable = (match Assoc_list.search "VARIABLE" fields with Some (_::variable::_) -> variable | _ -> failwith "missing variable");
            value = (match Assoc_list.search "VALUE" inputs with Some value -> value | None -> failwith "missing value");
        }
        | "data_addtolist" -> AddToList {
            id = id;
            next = next;
            list = (match Assoc_list.search "LIST" fields with Some (_::list::_) -> list | _ -> failwith "missing LIST field");
            item = (match Assoc_list.search "ITEM" inputs with Some item -> item | None -> failwith "missing ITEM for list");
        }
        | "data_deletealloflist" -> DeleteAllOfList {
            id = id;
            next = next;
            list = (match Assoc_list.search "LIST" fields with Some (_::list::_) -> list | _ -> failwith "missing LIST field");
        }
        | "data_itemnumoflist" -> NumOfList {
            id = id;
            list = (match Assoc_list.search "LIST" fields with Some (_::list::_) -> list | _ -> failwith "missing LIST field");
            item = (match Assoc_list.search "ITEM" inputs with Some item -> item | None -> failwith "missing ITEM for list");
        }
        | "data_changevariableby" -> ChangeVariableBy {
            id = id;
            next = next;
            value = (match Assoc_list.search "VALUE" inputs with Some value -> value | None -> failwith "missing value");
            variable = (match Assoc_list.search "VARIABLE" fields with Some (_::variable::_) -> variable | _ -> failwith "missing variable");
        }
        | "data_itemoflist" -> ItemOfList {
            id = id;
            list = (match Assoc_list.search "LIST" fields with Some (_::list::_) -> list | _ -> failwith "missing LIST field");
            index = (match Assoc_list.search "INDEX" inputs with Some value -> value | None -> failwith "missing index");
        }
        | "data_replaceitemoflist" -> ReplaceItemOfList {
            id = id;
            next = next;
            list = (match Assoc_list.search "LIST" fields with Some (_::list::_) -> list | _ -> failwith "missing LIST field");
            index = (match Assoc_list.search "INDEX" inputs with Some item -> item | None -> failwith "missing ITEM for list");
            item = (match Assoc_list.search "ITEM" inputs with Some value -> value | None -> failwith "missing index");
        }
        | "data_lengthoflist" -> LengthOfList {
            id = id;
            list = (match Assoc_list.search "LIST" fields with Some (_::list::_) -> list | _ -> failwith "missing LIST field");
        }
        | "control_repeat_until" -> RepeatUntil {
            id = id;
            next = next;
            condition = (match Assoc_list.search "CONDITION" inputs with Some cond -> cond | None -> failwith "missing condition");
            body = (match Assoc_list.search "SUBSTACK" inputs with Some (Id body) -> Some body | _ -> None); 
        }
        | "control_repeat" -> Repeat {
            id = id;
            next = next;
            count = (match Assoc_list.search "TIMES" inputs with Some cond -> cond | None -> failwith "missing times");
            body = (match Assoc_list.search "SUBSTACK" inputs with Some (Id body) -> Some body | _ -> None); 
        }
        | "looks_say" -> Say {
            id = id;
            next = next;
            message = (match Assoc_list.search "MESSAGE" inputs with Some message -> message | None -> failwith "missing message");
        }
        | "sensing_askandwait" -> Ask {
            id = id;
            next = next;
            question = (match Assoc_list.search "QUESTION" inputs with Some question -> question | None -> failwith "missing question");
        }
        | "sensing_answer" -> Answer {
            id = id;
        }
        | opcode -> failwith @@ "invalid opcode: " ^ opcode

let convert past = {
    variables = VariableExtractor.extract past;
    lists = ListExtractor.extract past;
    blocks = BlockExtractor.extract past |> List.map general_block_to_block
}
