module StringMap : Map.S with type key = string

module JsonMap : sig
  type 'a t = 'a StringMap.t [@@deriving show, yojson]
end

type primitive_value = Scratch_value.primitive_value [@@deriving show, yojson]

type input =
  | Id of string
  | Variable of string
  | Value of Scratch_value.primitive_value
[@@deriving show]

type input_opt = input option [@@deriving show, yojson]

type mutation = {proccode: string} [@@deriving show, yojson {strict= false}]

type block =
  { opcode: string
  ; next: string option
  ; (*  parent: string option;*)
    inputs: input_opt JsonMap.t
  ; fields: (string * string option) JsonMap.t
  ; mutation: mutation option [@default None] }
[@@deriving show, yojson]

type variable = string * primitive_value
[@@deriving show, yojson {strict= false}]

type target =
  { is_stage: bool
  ; name: string
  ; variables: variable JsonMap.t
  ; lists: (string * primitive_value list) JsonMap.t
  ; blocks: block JsonMap.t
  ; current_costume: int
  ; costumes: Costume.t list
  ; x: float
  ; y: float
  ; direction: float }
[@@deriving show, yojson {strict= false}]

type program = {targets: target list}
[@@deriving show, yojson {exn= true; strict= false}]

val parse : string -> program
