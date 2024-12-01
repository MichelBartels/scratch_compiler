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

let rec statements_of_block parameter_mapping costumes backdrops block =
  let next = statements_of_block_opt parameter_mapping costumes backdrops in
  match block with
  | ProceduresCall call ->
      Untyped_ast.FuncCall
        ( call.proccode
        , call.inputs
          |> List.map (fun (arg_id, input) ->
                 ( Parse.StringMap.find arg_id parameter_mapping
                 , expr_of_block input ) )
          |> Parse.StringMap.of_list )
      :: next call.next
  | IfThenElse br ->
      Untyped_ast.Branch
        (expr_of_block br.condition, next br.then_branch, next br.else_branch)
      :: next br.next
  | SetVariable set ->
      Untyped_ast.SetVariable (set.variable, expr_of_block set.value)
      :: next set.next
  | AddToList add ->
      Untyped_ast.AddToList (add.list, expr_of_block add.item) :: next add.next
  | DeleteAllOfList del ->
      Untyped_ast.DeleteAllOfList del.list :: next del.next
  | ChangeVariableBy c ->
      Untyped_ast.IncrVariable (c.variable, expr_of_block c.value)
      :: next c.next
  | ReplaceItemOfList r ->
      Untyped_ast.SetIndex
        {list= r.list; index= expr_of_block r.index; value= expr_of_block r.item}
      :: next r.next
  | RepeatUntil r ->
      Untyped_ast.WhileNot (expr_of_block r.condition, next r.body)
      :: next r.next
  | Repeat r ->
      Untyped_ast.Repeat (expr_of_block r.count, next r.body) :: next r.next
  | Say s ->
      Untyped_ast.Say (expr_of_block s.message) :: next s.next
  | SayForSeconds s ->
      Untyped_ast.SayForSeconds
        {message= expr_of_block s.message; duration= expr_of_block s.duration}
      :: next s.next
  | Think s ->
      Untyped_ast.Think (expr_of_block s.message) :: next s.next
  | ThinkForSeconds s ->
      Untyped_ast.ThinkForSeconds
        {message= expr_of_block s.message; duration= expr_of_block s.duration}
      :: next s.next
  | SwitchCostume {next= next'; costume= Costume c} ->
      Untyped_ast.SwitchCostume
        ( List.find_index (fun (x : Costume.t) -> x.name = c) costumes
        |> Option.get )
      :: next next'
  | NextCostume n ->
      Untyped_ast.NextCostume :: next n.next
  | SwitchBackdrop {next= next'; backdrop= Backdrop b} ->
      Untyped_ast.SwitchBackdrop
        ( List.find_index (fun (x : Costume.t) -> x.name = b) backdrops
        |> Option.get )
      :: next next'
  | NextBackdrop n ->
      Untyped_ast.NextBackdrop :: next n.next
  | ChangeSizeBy c ->
      Untyped_ast.ChangeSizeBy (expr_of_block c.size) :: next c.next
  | Show s ->
      Untyped_ast.Show :: next s.next
  | Hide h ->
      Untyped_ast.Hide :: next h.next
  | GoForwardBackwardLayers g ->
      Untyped_ast.GoForwardBackwardLayers (expr_of_block g.layers, g.direction)
      :: next g.next
  | GoToFrontBack g ->
      Untyped_ast.GoToFrontBack g.front_back :: next g.next
  | Ask a ->
      Untyped_ast.Ask (expr_of_block a.question) :: next a.next
  | SetX x ->
      Untyped_ast.SetX (expr_of_block x.x) :: next x.next
  | SetY y ->
      Untyped_ast.SetY (expr_of_block y.y) :: next y.next
  | ChangeXBy x ->
      Untyped_ast.ChangeX (expr_of_block x.x) :: next x.next
  | ChangeYBy y ->
      Untyped_ast.ChangeY (expr_of_block y.y) :: next y.next
  | GoToXY g ->
      Untyped_ast.GoToXY {x= expr_of_block g.x; y= expr_of_block g.y}
      :: next g.next
  | GoTo {target= GoToMenu target; next= next'} ->
      Untyped_ast.GoTo target :: next next'
  | TurnRight d ->
      Untyped_ast.TurnRight (expr_of_block d.degrees) :: next d.next
  | TurnLeft d ->
      Untyped_ast.TurnLeft (expr_of_block d.degrees) :: next d.next
  | MoveSteps steps ->
      Untyped_ast.MoveSteps (expr_of_block steps.steps) :: next steps.next
  | GlideToXY g ->
      Untyped_ast.GlideToXY
        { x= expr_of_block g.x
        ; y= expr_of_block g.y
        ; duration= expr_of_block g.duration }
      :: next g.next
  | GlideTo {target= GlideToMenu target; next= next'; duration} ->
      Untyped_ast.GlideTo {target; duration= expr_of_block duration}
      :: next next'
  | PointTowards {target= PointTowardsMenu target; next= next'} ->
      Untyped_ast.PointTowards target :: next next'
  | IfOnEdgeBounce i ->
      Untyped_ast.IfOnEdgeBounce :: next i.next
  | SetRotationStyle r ->
      Untyped_ast.SetRotationStyle r.style :: next r.next
  | block ->
      failwith @@ "block is not a valid statement" ^ show_block block

and statements_of_block_opt parameter_mapping costumes backdrops stmt =
  Option.map (statements_of_block parameter_mapping costumes backdrops) stmt
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

let create_function global_parameter_mapping parameter_mapping costumes
    backdrops next =
  Untyped_ast.
    { parameters= Parse.StringMap.bindings parameter_mapping |> List.map snd
    ; code=
        statements_of_block_opt global_parameter_mapping costumes backdrops next
    }

let create_entrypoints parameter_mapping backdrops sprite =
  List.filter_map
    (function
      | Start start ->
          Some
            (statements_of_block_opt parameter_mapping sprite.costumes backdrops
               start.next )
      | _ ->
          None )
    sprite.blocks

let union_exn = Parse.StringMap.union (fun _ _ -> failwith "duplicate key")

let convert_sprite backdrops sprite =
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
        ( code
        , create_function global_parameter_mapping mapping sprite.costumes
            backdrops next ) )
      procedures
  in
  Untyped_ast.
    { functions= Parse.StringMap.of_list functions
    ; variables= sprite.variables
    ; entry_points= create_entrypoints global_parameter_mapping backdrops sprite
    ; current_costume= sprite.current_costume
    ; costumes= sprite.costumes
    ; name= sprite.name
    ; x= sprite.x
    ; y= sprite.y
    ; direction= sprite.direction
    ; rotation_style= sprite.rotation_style
    ; is_stage= sprite.is_stage }

let convert (program : program) =
  let backdrops =
    List.find_map
      (fun s -> if s.is_stage then Some s.costumes else None)
      program.sprites
    |> Option.get
  in
  Untyped_ast.
    { sprites= List.map (convert_sprite backdrops) program.sprites
    ; globals= program.globals }
