type t =
    | Boolean
    | Float
    | String
[@@ deriving show]

let to_lltype = function
    | Boolean -> Llvm.i1_type
    | Float -> Llvm.double_type
    | String -> Llvm.pointer_type
