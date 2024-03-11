open Blocks

let rec find_block k = function
    | (Argument arg)::_ when arg.id = k -> Argument arg
    | (ProceduresDefinition def)::_ when def.id = k -> ProceduresDefinition def
    | (ProceduresPrototype prot)::_ when prot.id = k -> ProceduresPrototype prot
    | (BinaryOperator op)::_ when op.id = k -> BinaryOperator op
    | (Not not)::_ when not.id = k -> Not not
    | (ProceduresCall proc)::_ when proc.id = k -> ProceduresCall proc
    | (Start s)::_ when s.id = k -> Start s
    | (IfThenElse branch)::_ when branch.id = k -> IfThenElse branch
    | (SetVariable set)::_ when set.id = k -> SetVariable set
    | (AddToList add)::_ when add.id = k -> AddToList add
    | (DeleteAllOfList del)::_ when del.id = k -> DeleteAllOfList del
    | (NumOfList num)::_ when num.id = k -> NumOfList num
    | (ChangeVariableBy change)::_ when change.id = k -> ChangeVariableBy change
    | (ItemOfList i)::_ when i.id = k -> ItemOfList i
    | (ReplaceItemOfList r)::_ when r.id = k -> ReplaceItemOfList r
    | (LengthOfList len)::_ when len.id = k -> LengthOfList len
    | (RepeatUntil rep)::_ when rep.id = k -> RepeatUntil rep
    | (Repeat rep)::_ when rep.id = k -> Repeat rep
    | (Say say)::_ when say.id = k -> Say say
    | (Ask ask)::_ when ask.id = k -> Ask ask
    | (Answer answer)::_ when answer.id = k -> Answer answer
    | _::xs -> find_block k xs
    | [] -> failwith @@ "could not find block: " ^ k

let rec get_arg_name_by_arg_id id = function
    | (Argument arg)::_ when arg.id = id -> arg.name
    | _::xs -> get_arg_name_by_arg_id id xs
    | [] -> failwith "could not find argument by argument id"

let get_arg_name_by_input_id id blocks =
    let rec loop = function
        | (ProceduresPrototype prot)::xs -> (match Assoc_list.search id prot.parameters with
            | Some id -> get_arg_name_by_arg_id id blocks
            | None -> loop xs)
        | _::xs -> loop xs
        | [] -> failwith "could not find argument by input id"
    in loop blocks

let rec get_name_by_id variables id = match variables with
    | (var: variable)::_ when var.id = id -> var.name
    | _::vars -> get_name_by_id vars id
    | [] -> failwith @@ "could not find variable " ^ id

let assert_single_expr = function
    | [expr] -> expr
    | _ -> failwith "can only have one expr as return type is used"

let rec expr_of_input program = function
    | Id id ->  Some id |> exprs_of_string_opt program |> assert_single_expr
    | Value v -> Untyped_ast.Literal v
    | Variable v -> Untyped_ast.Variable (get_name_by_id program.variables v)
and exprs_of_string_opt program id =
    let expr_of_input = expr_of_input program
    in let exprs_of_string_opt = exprs_of_string_opt program
    in let get_variable_name_by_id = get_name_by_id program.variables
    in let get_list_name_by_id = get_name_by_id program.lists
    in Untyped_ast.(match id with
    | Some str -> (match find_block str program.blocks with
        | Argument arg -> [Argument arg.name]
        | BinaryOperator op -> [BinaryOperator (op.operator, expr_of_input op.arg1, expr_of_input op.arg2)]
        | Not not -> [Not (expr_of_input not.arg)]
        | ProceduresCall call -> FuncCall (call.proccode, List.map (fun (str, input) -> (get_arg_name_by_input_id str program.blocks, expr_of_input input)) call.inputs) :: (exprs_of_string_opt call.next)
        | IfThenElse branch -> (Branch (Some branch.condition |> exprs_of_string_opt |> assert_single_expr, exprs_of_string_opt branch.then_branch, exprs_of_string_opt branch.else_branch)) :: (exprs_of_string_opt branch.next)
        | SetVariable set -> (SetVariable (get_variable_name_by_id set.variable, expr_of_input set.value ) :: (exprs_of_string_opt set.next))
        | AddToList add -> (AddToList (get_list_name_by_id add.list, expr_of_input add.item) :: (exprs_of_string_opt add.next))
        | DeleteAllOfList del -> (DeleteAllOfList (get_list_name_by_id del.list)) :: (exprs_of_string_opt del.next)
        | NumOfList num -> [IndexOf (get_list_name_by_id num.list, expr_of_input num.item)]
        | ChangeVariableBy c -> (IncrVariable (get_variable_name_by_id c.variable, expr_of_input c.value)) :: (exprs_of_string_opt c.next)
        | ItemOfList i -> [Index (get_list_name_by_id i.list, expr_of_input i.index)]
        | ReplaceItemOfList r -> (SetIndex {
            list = get_list_name_by_id r.list;
            index = expr_of_input r.index;
            value = expr_of_input r.item
        }) :: (exprs_of_string_opt r.next)
        | LengthOfList l -> [Length (get_list_name_by_id l.list)]
        | RepeatUntil r -> (WhileNot (expr_of_input r.condition, exprs_of_string_opt r.body)) :: (exprs_of_string_opt r.next)
        | Repeat r -> (Repeat (expr_of_input r.count, exprs_of_string_opt r.body)) :: (exprs_of_string_opt r.next)
        | Say s -> (Say (expr_of_input s.message)) :: (exprs_of_string_opt s.next)
        | Ask a -> (Ask (expr_of_input a.question)) :: (exprs_of_string_opt a.next)
        | Answer _ -> [Answer]
        | expr -> failwith @@ "unsupported expression: " ^ show_block expr)
    | None -> [])

let create_functions program =
    let rec loop = function
        | (ProceduresDefinition def)::xs ->
                (match find_block def.prototype program.blocks with
                    | ProceduresPrototype prot -> (prot.proccode, Untyped_ast.({
                        parameters = List.map (fun (_, id) -> get_arg_name_by_arg_id id program.blocks) prot.parameters;
                        statements = exprs_of_string_opt program def.next;
                    }))::(loop xs)
                    | _ -> failwith "invalid prototype type")
        | _::xs -> loop xs
        | [] -> []
    in loop program.blocks

let create_main program =
    let rec loop = function
        | (Start s)::_ -> exprs_of_string_opt program s.next
        | _::xs -> loop xs
        | [] -> failwith "program is missing a start block"
    in loop program.blocks

let convert program = Untyped_ast.({
        functions = create_functions program;
        variables = List.map (fun (var: variable) -> (var.name, var.value)) program.variables;
        lists = List.map (fun (var: variable) -> (var.name, var.value)) program.lists;
        main = create_main program
    })
