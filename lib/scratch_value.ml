open Scratch_type

type t =
    | NumberValue of float
    | BoolValue of bool
    | StringValue of string
    | ListValue of t list
[@@ deriving show]

let list_to_string l = List.fold_right (fun x y -> x ^ y) l ""

let rec cast t v = match v with
    | NumberValue n -> (match t with
        | Boolean -> failwith "cannot cast to boolean"
        | Float -> NumberValue n
        | String -> StringValue (match modf n with
            | (0., _) -> string_of_int (int_of_float n)
            | _ -> string_of_float n)
    )
    | BoolValue b -> (match t with
        | Boolean -> BoolValue b
        | Float -> NumberValue (if b then 1. else 0.)
        | String -> StringValue (if b then "true" else "false")
    )
    | StringValue str -> (match t with
        | Boolean -> failwith "cannot cast to boolean"
        | Float -> NumberValue (match float_of_string_opt str with
            | Some n -> n
            | None -> 0.
        )
        | String -> StringValue str
    )
    | ListValue l -> (let str = List.map (cast t) l |> List.map (fun x -> match x with StringValue str -> str | _ -> failwith "invalid output") |> list_to_string in match t with
        | Boolean -> failwith "cannot cast to boolean"
        | Float -> NumberValue (match float_of_string_opt str with
            | Some f -> f
            | None -> 0.
        )
        | String -> StringValue str)

let get_type = function
    | NumberValue _ -> Float
    | BoolValue _ -> Boolean
    | StringValue _ -> String
    | ListValue _ -> String

let default = function
    | Float -> NumberValue 0.
    | Boolean -> BoolValue false
    | String -> StringValue ""
