type primitive_type =
    | Boolean
    | Float
    | String
[@@ deriving show]

type t =
    | Primitive of primitive_type
    | List of primitive_type
[@@ deriving show]

let to_lltype = function
    | Primitive Boolean -> Llvm.i1_type
    | Primitive Float -> Llvm.double_type
    | Primitive String -> Llvm.pointer_type
    | List _ -> Llvm.pointer_type

let unify_primitive a b =
    match a, b with
    | String, _ | _, String -> String
    | Float, _ | _, Float -> Float
    | Boolean, Boolean -> Boolean

let unify a b =
    match a, b with
    | Primitive a, Primitive b -> Primitive (unify_primitive a b)
    | List a, List b -> List (unify_primitive a b)
    | _ -> Primitive String

let bottom_primitive = Boolean

