type primitive_value =
    | Float of float
    | Boolean of bool
    | String of string
[@@ deriving show]

type t =
    | Primitive of primitive_value
    | List of primitive_value list
[@@ deriving show]

let list_to_string l = List.fold_right (fun x y -> x ^ y) l ""

let cast_primitive_value t v = match v with
    | Float n -> (match t with
        | Scratch_type.Boolean -> failwith "cannot cast to boolean"
        | Float -> Float n
        | String -> String (match modf n with
            | (0., _) -> string_of_int (int_of_float n)
            | _ -> string_of_float n)
    )
    | Boolean b -> (match t with
        | Scratch_type.Boolean -> Boolean b
        | Float -> Float (if b then 1. else 0.)
        | String -> String (if b then "true" else "false")
    )
    | String str -> (match t with
        | Boolean -> failwith "cannot cast to boolean"
        | Float -> Float (match float_of_string_opt str with
            | Some n -> n
            | None -> 0.
        )
        | String -> String str
    )

let cast t = function
    | Primitive v -> Primitive (cast_primitive_value t v)
    | List l -> List (List.map (cast_primitive_value t) l)

let get_primitive_type = function
    | Float _ -> Scratch_type.Float
    | Boolean _ -> Boolean
    | String _ -> String

let get_primitive_list_type ls = List.fold_left Scratch_type.unify_primitive Scratch_type.bottom_primitive (List.map get_primitive_type ls)

let get_type = function
    | Primitive v -> Scratch_type.Primitive (get_primitive_type v)
    | List vs -> Scratch_type.List (get_primitive_list_type vs)

let default = function
    | Scratch_type.Primitive t -> Primitive (match t with
        | Scratch_type.Float -> Float 0.
        | Scratch_type.Boolean -> Boolean false
        | Scratch_type.String -> String ""
    )
    | Scratch_type.List _ -> List []
