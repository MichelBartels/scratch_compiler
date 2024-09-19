module StringMap = Map.Make (String)

module JsonMap : sig
  type 'a t = 'a StringMap.t [@@deriving show, yojson]
end = struct
  type 'a t = 'a StringMap.t

  let to_yojson yojson_of_a m =
    let l = StringMap.bindings m in
    `Assoc (List.map (fun (k, v) -> (k, yojson_of_a v)) l)

  let of_yojson a_of_yojson = function
    | `Assoc l ->
        List.fold_left
          (fun acc (k, v) ->
            Result.bind acc (fun acc ->
                Result.bind (a_of_yojson v) (fun v ->
                    Ok (StringMap.add k v acc) ) ) )
          (Ok StringMap.empty) l
    | _ ->
        Error "JsonMap.of_yojson: expected `Assoc"

  let pp pp_a fmt m =
    let l = StringMap.bindings m in
    Format.fprintf fmt "{" ;
    List.iter (fun (k, v) -> Format.fprintf fmt "%s: %a; " k pp_a v) l ;
    Format.fprintf fmt "}"

  let show pp' m =
    pp pp' Format.str_formatter m ;
    Format.flush_str_formatter ()
end

type primitive_value = Scratch_value.primitive_value [@@deriving show]

let primitive_value_to_yojson = function
  | Scratch_value.String s ->
      `String s
  | Scratch_value.Float f ->
      `Float f
  | Scratch_value.Boolean b ->
      `Bool b

let primitive_value_of_yojson = function
  | `String s ->
      Ok (Scratch_value.String s)
  | `Float f ->
      Ok (Scratch_value.Float f)
  | `Bool b ->
      Ok (Scratch_value.Boolean b)
  | `Int i ->
      Ok (Scratch_value.Float (float_of_int i))
  | _ ->
      Error "primitive_value_of_yojson: expected `String, `Bool, `Int or `Float"

type input =
  | Id of string
  | Variable of string
  | Value of Scratch_value.primitive_value
[@@deriving show]

type input_opt = input option [@@deriving show]

let input_opt_of_yojson = function
  | `List (_ :: `List [`Int m; `String n] :: _) when m >= 4 && m <= 8 ->
      Ok (Some (Value (Float (float_of_string n))))
  | `List (_ :: `List [`Int 10; `String str] :: _) ->
      Ok
        (Some
           ( match float_of_string_opt str with
           | Some f ->
               Value (Float f)
           | None ->
               Value (String str) ) )
  | `List (_ :: `String id :: _) ->
      Ok (Some (Id id))
  | `List (_ :: `List [`Int 12; _; `String id] :: _) ->
      Ok (Some (Variable id))
  | `List (_ :: `Null :: _) ->
      Ok None
  | json ->
      Error
        ( "input_of_yojson: expected `List, got instead "
        ^ Yojson.Safe.to_string json )

let input_opt_to_yojson _ = failwith "input_to_yojson: not implemented"

type mutation = {proccode: string} [@@deriving show, yojson {strict= false}]

type block =
  { opcode: string
  ; next: string option
  ; (*  parent: string option;*)
    inputs: input_opt JsonMap.t
  ; fields: (string * string option) JsonMap.t
  ; mutation: mutation option [@default None] }
[@@deriving show, yojson {strict= false}]

type variable = string * primitive_value
[@@deriving show, yojson {strict= false}]

type target =
  { is_stage: bool [@key "isStage"]
  ; name: string
  ; variables: variable JsonMap.t
  ; lists: (string * primitive_value list) JsonMap.t
  ; blocks: block JsonMap.t
  ; current_costume: int [@key "currentCostume"]
  ; costumes: Costume.t list
  ; x: float [@default 0.]
  ; y: float [@default 0.]
  ; direction: float [@default 90.] }
[@@deriving show, yojson {strict= false}]

type program = {targets: target list}
[@@deriving show, yojson {exn= true; strict= false}]

let parse f = Yojson.Safe.from_file f |> program_of_yojson_exn
