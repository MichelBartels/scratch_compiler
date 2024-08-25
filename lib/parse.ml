module StringMap = Map.Make(String)
module JsonMap: sig
  type 'a t = 'a StringMap.t [@@deriving show, yojson]
end = struct
  type 'a t = 'a StringMap.t

  let to_yojson yojson_of_a m =
    let l = StringMap.bindings m in
    `Assoc (List.map (fun (k, v) -> (k, yojson_of_a v)) l)
  let of_yojson a_of_yojson = function
    | `Assoc l ->
      List.fold_left (fun acc (k, v) ->
          Result.bind acc (fun acc ->
              Result.bind (a_of_yojson v) (fun v ->
                  Ok (StringMap.add k v acc)))
        ) (Ok StringMap.empty) l
    | _ -> Error "JsonMap.of_yojson: expected `Assoc"

  let pp pp_a fmt m =
    let l = StringMap.bindings m in
    Format.fprintf fmt "{";
    List.iter (fun (k, v) -> Format.fprintf fmt "%s: %a; " k pp_a v) l;
    Format.fprintf fmt "}"

  let show pp' m =
    pp pp' Format.str_formatter m;
    Format.flush_str_formatter ()
end
type value =
  | String of string
  | Float of float
  | Int of int
[@@deriving show]

let value_to_yojson = function
  | String s -> `String s
  | Float f -> `Float f
  | Int i -> `Int i

let value_of_yojson = function
  | `String s -> Result.ok @@ String s
  | `Float f -> Result.ok @@ Float f
  | `Int i -> Result.ok @@ Int i
  | _ ->  Result.error "value_of_yojson: expected `String or `Float"

type input =
    | Id of string
    | Variable of string
    | Value of Scratch_value.t
[@@ deriving show]

let create_value = function
    | Past.Array(_, _::(Past.Array (_, [Past.Number (_, m); Past.String (_, n)]))::_) when m >= 4. && m <= 8. -> Some (Value (Primitive (Float (float_of_string n))))
    | Past.Array(_, _::(Past.Array (_, [Past.Number (_, 10.); Past.String (_, str)]))::_) -> Some (match float_of_string_opt str with
        | Some f -> Value (Primitive (Float f))
        | None -> Value (Primitive (String str))
    )
    | Past.Array(_, _::Past.String(_, id)::_) -> Some (Id id)
    | Past.Array(_, _::(Past.Array (_, [Past.Number (_, 12.); _; Past.String (_, id)]))::_) -> Some (Variable id)
    | _ -> None

let input_of_yojson = function
  | `List (_::`List [`Int m; `String n]::_) when m >= 4 && m <= 8 -> Ok (Value (Scratch_value.Primitive (Float (float_of_string n))))
  | `List (_::`List [`Int 10; `String str]::_) -> (match float_of_string_opt str with
      | Some f -> Ok (Value (Scratch_value.Primitive (Float f)))
      | None -> Ok (Value (Scratch_value.Primitive (String str)))
    )
  | `List (_::`String id::_) -> Ok (Id id)
  | `List (_::`List [`Int 12; _; `String id]::_) -> Ok (Variable id)
  | json -> Error ("input_of_yojson: expected `List, got instead " ^ Yojson.Safe.to_string json)

let input_to_yojson _ = failwith "input_to_yojson: not implemented"

type block = {
  opcode: string;
  next: string option;
  parent: string option;
  inputs: input JsonMap.t;
  fields: (string * string option) JsonMap.t;
}
[@@deriving show, yojson { strict = false }]

type variable = string * value
[@@deriving show, yojson { strict = false }]

type target = {
  is_stage: bool [@key "isStage"];
  name: string;
  variables: variable JsonMap.t;
  lists: (string * value list) JsonMap.t;
  blocks: block JsonMap.t;
}
[@@deriving show, yojson { strict = false }]

type program = {
  targets: target list;
}
[@@deriving show, yojson { exn = true; strict = false }]

let parse f =
  Yojson.Safe.from_file f |> program_of_yojson_exn
