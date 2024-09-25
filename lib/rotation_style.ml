type t = AllAround | LeftRight | DontRotate [@@deriving show]

let of_string = function
  | "all around" ->
      AllAround
  | "left-right" ->
      LeftRight
  | "don't rotate" ->
      DontRotate
  | _ ->
      failwith "Invalid rotation type"

let to_int = function AllAround -> 0 | LeftRight -> 1 | DontRotate -> 2
