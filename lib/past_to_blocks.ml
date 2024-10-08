open Blocks
open Parse
open Stdlib

let get_variables sprite =
  let lists =
    StringMap.map (fun (_, values) -> Scratch_value.List values) sprite.lists
  in
  let variables =
    StringMap.map
      (fun (_, value) -> Scratch_value.Primitive value)
      sprite.variables
  in
  StringMap.union
    (fun _ _ -> failwith "List and variable with same id")
    variables lists

let partition_targets targets =
  let stages, sprites =
    List.partition (fun target -> target.is_stage) targets
  in
  (List.hd stages, sprites)

let parse_target target =
  let blocks = ref StringMap.empty in
  let rec create_block id =
    match StringMap.find_opt id !blocks with
    | Some block ->
        block
    | None ->
        let {opcode; next; inputs; fields; mutation} =
          StringMap.find id target.blocks
        in
        let next = Option.map (fun next -> create_block next) next in
        let proccode =
          Option.map (fun mutation -> mutation.proccode) mutation
        in
        let block =
          match opcode with
          | "procedures_definition" ->
              Blocks.ProceduresDefinition
                { next
                ; prototype=
                    ( match StringMap.find_opt "custom_block" inputs with
                    | Some (Some (Id prot)) ->
                        create_block prot
                    | _ ->
                        failwith "invalid procedures_definition declaration" )
                }
          | "procedures_prototype" ->
              ProceduresPrototype
                { parameters=
                    StringMap.map input_to_block inputs
                    |> StringMap.map Option.get
                ; proccode= Option.get proccode }
          | "argument_reporter_string_number" ->
              Argument {name= StringMap.find "VALUE" fields |> fst}
          | "operator_gt" ->
              create_op_block inputs Untyped_ast.Gt "OPERAND1" "OPERAND2"
          | "operator_lt" ->
              create_op_block inputs Lt "OPERAND1" "OPERAND2"
          | "operator_equals" ->
              create_op_block inputs Equals "OPERAND1" "OPERAND2"
          | "operator_or" ->
              create_op_block inputs Or "OPERAND1" "OPERAND2"
          | "operator_subtract" ->
              create_op_block inputs Subtract "NUM1" "NUM2"
          | "operator_add" ->
              create_op_block inputs Add "NUM1" "NUM2"
          | "operator_join" ->
              create_op_block inputs Join "STRING1" "STRING2"
          | "operator_letter_of" ->
              create_op_block inputs LetterOf "LETTER" "STRING"
          | "operator_not" ->
              Not {arg= input_field_to_block inputs "OPERAND"}
          | "procedures_call" ->
              ProceduresCall
                { next
                ; inputs=
                    StringMap.bindings inputs
                    |> List.map (fun (name, input) ->
                           (name, Option.get (input_to_block input)) )
                ; proccode= Option.get proccode }
          | "event_whenflagclicked" ->
              Start {next}
          | "control_if_else" ->
              IfThenElse
                { next
                ; condition= input_field_to_block inputs "CONDITION"
                ; then_branch= input_field_to_block_opt inputs "SUBSTACK"
                ; else_branch= input_field_to_block_opt inputs "SUBSTACK2" }
          | "control_if" ->
              IfThenElse
                { next
                ; condition= input_field_to_block inputs "CONDITION"
                ; then_branch= input_field_to_block_opt inputs "SUBSTACK"
                ; else_branch= None }
          | "data_setvariableto" ->
              SetVariable
                { next
                ; variable= extract_field fields "VARIABLE"
                ; value= input_field_to_block inputs "VALUE" }
          | "data_addtolist" ->
              AddToList
                { next
                ; list= extract_field fields "LIST"
                ; item= input_field_to_block inputs "ITEM" }
          | "data_deletealloflist" ->
              DeleteAllOfList {next; list= extract_field fields "LIST"}
          | "data_itemnumoflist" ->
              NumOfList
                { list= extract_field fields "LIST"
                ; item= input_field_to_block inputs "ITEM" }
          | "data_changevariableby" ->
              ChangeVariableBy
                { next
                ; value= input_field_to_block inputs "VALUE"
                ; variable= extract_field fields "VARIABLE" }
          | "data_itemoflist" ->
              ItemOfList
                { list= extract_field fields "LIST"
                ; index= input_field_to_block inputs "INDEX" }
          | "data_replaceitemoflist" ->
              ReplaceItemOfList
                { next
                ; list= extract_field fields "LIST"
                ; index= input_field_to_block inputs "INDEX"
                ; item= input_field_to_block inputs "ITEM" }
          | "data_lengthoflist" ->
              LengthOfList {list= extract_field fields "LIST"}
          | "control_repeat_until" ->
              RepeatUntil
                { next
                ; condition= input_field_to_block inputs "CONDITION"
                ; body= input_field_to_block_opt inputs "SUBSTACK" }
          | "control_repeat" ->
              Repeat
                { next
                ; count= input_field_to_block inputs "TIMES"
                ; body= input_field_to_block_opt inputs "SUBSTACK" }
          | "looks_say" ->
              Say {next; message= input_field_to_block inputs "MESSAGE"}
          | "sensing_askandwait" ->
              Ask {next; question= input_field_to_block inputs "QUESTION"}
          | "sensing_answer" ->
              Answer
          | "motion_setx" ->
              SetX {next; x= input_field_to_block inputs "X"}
          | "motion_sety" ->
              SetY {next; y= input_field_to_block inputs "Y"}
          | "motion_changexby" ->
              ChangeXBy {next; x= input_field_to_block inputs "DX"}
          | "motion_changeyby" ->
              ChangeYBy {next; y= input_field_to_block inputs "DY"}
          | "motion_gotoxy" ->
              GoToXY
                { next
                ; x= input_field_to_block inputs "X"
                ; y= input_field_to_block inputs "Y" }
          | "motion_goto" ->
              GoTo {next; target= input_field_to_block inputs "TO"}
          | "motion_goto_menu" ->
              GoToMenu StringMap.(find "TO" fields |> fst)
          | "motion_turnright" ->
              TurnRight {next; degrees= input_field_to_block inputs "DEGREES"}
          | "motion_turnleft" ->
              TurnLeft {next; degrees= input_field_to_block inputs "DEGREES"}
          | "motion_xposition" ->
              XPosition
          | "motion_yposition" ->
              YPosition
          | "motion_direction" ->
              Direction
          | "motion_movesteps" ->
              MoveSteps {next; steps= input_field_to_block inputs "STEPS"}
          | "motion_glidesecstoxy" ->
              GlideToXY
                { next
                ; x= input_field_to_block inputs "X"
                ; y= input_field_to_block inputs "Y"
                ; duration= input_field_to_block inputs "SECS" }
          | "motion_glideto" ->
              GlideTo
                { next
                ; target= input_field_to_block inputs "TO"
                ; duration= input_field_to_block inputs "SECS" }
          | "motion_glideto_menu" ->
              GlideToMenu StringMap.(find "TO" fields |> fst)
          | "motion_pointtowards" ->
              PointTowards {next; target= input_field_to_block inputs "TOWARDS"}
          | "motion_pointtowards_menu" ->
              PointTowardsMenu StringMap.(find "TOWARDS" fields |> fst)
          | "motion_ifonedgebounce" ->
              IfOnEdgeBounce {next}
          | "motion_setrotationstyle" ->
              SetRotationStyle
                { next
                ; style=
                    Rotation_style.of_string
                      (StringMap.find "STYLE" fields |> fst) }
          | opcode ->
              failwith @@ "invalid opcode: " ^ opcode
        in
        update_block id block
  and update_block id block =
    blocks := StringMap.add id block !blocks ;
    block
  and create_op_block inputs op arg1 arg2 =
    BinaryOperator
      { operator= op
      ; arg1= input_field_to_block inputs arg1
      ; arg2= input_field_to_block inputs arg2 }
  and input_to_block = function
    | Some (Id id) ->
        Some (create_block id)
    | Some (Variable var) ->
        Some (Blocks.Variable var)
    | Some (Value value) ->
        Some (Blocks.Constant value)
    | None ->
        None
  and input_field_to_block_opt inputs field =
    let input = StringMap.find_opt field inputs in
    Option.bind input input_to_block
  and input_field_to_block inputs field =
    Option.get (input_field_to_block_opt inputs field)
  and extract_field fields field =
    StringMap.find field fields |> snd |> Option.get
  in
  let variables = get_variables target in
  let blocks =
    List.map (fun (id, _) -> create_block id)
    @@ StringMap.bindings target.blocks
  in
  { variables
  ; blocks
  ; current_costume= target.current_costume
  ; costumes= target.costumes
  ; name= target.name
  ; x= target.x
  ; y= target.y
  ; direction= target.direction
  ; rotation_style=
      ( match target.rotation_style with
      | "all around" ->
          AllAround
      | "left-right" ->
          LeftRight
      | "don't rotate" ->
          DontRotate
      | _ ->
          failwith "invalid rotation style" ) }

let convert program =
  let target, sprites = partition_targets program.targets in
  let globals = get_variables target in
  let sprites = List.map parse_target sprites in
  {globals; sprites}
