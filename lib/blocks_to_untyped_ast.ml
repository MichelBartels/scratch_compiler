open Blocks
open Stdlib

let get_arg_name = function
  | Argument arg ->
      arg.name
  | _ ->
      failwith "could not find argument by arg id"

let rec expr_of_block = function
  | Constant c ->
      Untyped_ast.Literal (Scratch_value.Primitive c)
  | Variable v ->
      Untyped_ast.Variable v
  | Argument a ->
      Untyped_ast.Argument a.name
  | BinaryOperator b ->
      Untyped_ast.BinaryOperator
        (b.operator, expr_of_block b.arg1, expr_of_block b.arg2)
  | Not n ->
      Untyped_ast.Not (expr_of_block n.arg)
  | NumOfList n ->
      Untyped_ast.IndexOf (n.list, expr_of_block n.item)
  | ItemOfList i ->
      Untyped_ast.Index (i.list, expr_of_block i.index)
  | LengthOfList l ->
      Untyped_ast.Length l.list
  | Answer ->
      Untyped_ast.Answer
  | XPosition ->
      Untyped_ast.XPosition
  | YPosition ->
      Untyped_ast.YPosition
  | Direction ->
      Untyped_ast.Direction
  | _ ->
      failwith "block is not a valid expression"

let rec statements_of_block parameter_mapping = function
  | ProceduresCall call ->
      Untyped_ast.FuncCall
        ( call.proccode
        , call.inputs
          |> List.map (fun (arg_id, input) ->
                 ( Parse.StringMap.find arg_id parameter_mapping
                 , expr_of_block input ) )
          |> Parse.StringMap.of_list )
      :: statements_of_block_opt parameter_mapping call.next
  | IfThenElse br ->
      Untyped_ast.Branch
        ( expr_of_block br.condition
        , statements_of_block_opt parameter_mapping br.then_branch
        , statements_of_block_opt parameter_mapping br.else_branch )
      :: statements_of_block_opt parameter_mapping br.next
  | SetVariable set ->
      Untyped_ast.SetVariable (set.variable, expr_of_block set.value)
      :: statements_of_block_opt parameter_mapping set.next
  | AddToList add ->
      Untyped_ast.AddToList (add.list, expr_of_block add.item)
      :: statements_of_block_opt parameter_mapping add.next
  | DeleteAllOfList del ->
      Untyped_ast.DeleteAllOfList del.list
      :: statements_of_block_opt parameter_mapping del.next
  | ChangeVariableBy c ->
      Untyped_ast.IncrVariable (c.variable, expr_of_block c.value)
      :: statements_of_block_opt parameter_mapping c.next
  | ReplaceItemOfList r ->
      Untyped_ast.SetIndex
        {list= r.list; index= expr_of_block r.index; value= expr_of_block r.item}
      :: statements_of_block_opt parameter_mapping r.next
  | RepeatUntil r ->
      Untyped_ast.WhileNot
        ( expr_of_block r.condition
        , statements_of_block_opt parameter_mapping r.body )
      :: statements_of_block_opt parameter_mapping r.next
  | Repeat r ->
      Untyped_ast.Repeat
        (expr_of_block r.count, statements_of_block_opt parameter_mapping r.body)
      :: statements_of_block_opt parameter_mapping r.next
  | Say s ->
      Untyped_ast.Say (expr_of_block s.message)
      :: statements_of_block_opt parameter_mapping s.next
  | Ask a ->
      Untyped_ast.Ask (expr_of_block a.question)
      :: statements_of_block_opt parameter_mapping a.next
  | SetX x ->
      Untyped_ast.SetX (expr_of_block x.x)
      :: statements_of_block_opt parameter_mapping x.next
  | SetY y ->
      Untyped_ast.SetY (expr_of_block y.y)
      :: statements_of_block_opt parameter_mapping y.next
  | ChangeXBy x ->
      Untyped_ast.ChangeX (expr_of_block x.x)
      :: statements_of_block_opt parameter_mapping x.next
  | ChangeYBy y ->
      Untyped_ast.ChangeY (expr_of_block y.y)
      :: statements_of_block_opt parameter_mapping y.next
  | GoTo g ->
      Untyped_ast.GoTo {x= expr_of_block g.x; y= expr_of_block g.y}
      :: statements_of_block_opt parameter_mapping g.next
  | TurnRight d ->
      Untyped_ast.TurnRight (expr_of_block d.degrees)
      :: statements_of_block_opt parameter_mapping d.next
  | TurnLeft d ->
      Untyped_ast.TurnLeft (expr_of_block d.degrees)
      :: statements_of_block_opt parameter_mapping d.next
  | MoveSteps steps ->
      Untyped_ast.MoveSteps (expr_of_block steps.steps)
      :: statements_of_block_opt parameter_mapping steps.next
  | block ->
      failwith @@ "block is not a valid statement" ^ show_block block

and statements_of_block_opt parameter_mapping stmt =
  Option.map (statements_of_block parameter_mapping) stmt
  |> Option.value ~default:[]

let get_parameter_mapping = Parse.StringMap.map get_arg_name

let get_procedures =
  List.filter_map (function
    | ProceduresDefinition def ->
        Some
          ( def.next
          , def.prototype
            |> function
            | ProceduresPrototype prot ->
                (get_parameter_mapping prot.parameters, prot.proccode)
            | _ ->
                failwith "expected a prototype" )
    | _ ->
        None )

let create_function global_parameter_mapping parameter_mapping next =
  Untyped_ast.
    { parameters= Parse.StringMap.bindings parameter_mapping |> List.map snd
    ; code= statements_of_block_opt global_parameter_mapping next }

let create_entrypoints parameter_mapping sprite =
  List.filter_map
    (function
      | Start start ->
          Some (statements_of_block_opt parameter_mapping start.next)
      | _ ->
          None )
    sprite.blocks

let union_exn = Parse.StringMap.union (fun _ _ -> failwith "duplicate key")

let convert_sprite sprite =
  let procedures = get_procedures sprite.blocks in
  let parameter_mappings =
    List.map (fun (_, (mapping, _)) -> mapping) procedures
  in
  let global_parameter_mapping =
    List.fold_left union_exn Parse.StringMap.empty parameter_mappings
  in
  let functions =
    List.map
      (fun (next, (mapping, code)) ->
        (code, create_function global_parameter_mapping mapping next) )
      procedures
  in
  Untyped_ast.
    { functions= Parse.StringMap.of_list functions
    ; variables= sprite.variables
    ; entry_points= create_entrypoints global_parameter_mapping sprite
    ; current_costume= sprite.current_costume
    ; costumes= sprite.costumes
    ; x= sprite.x
    ; y= sprite.y
    ; direction= sprite.direction }

let convert (program : program) =
  Untyped_ast.
    {sprites= List.map convert_sprite program.sprites; globals= program.globals}
