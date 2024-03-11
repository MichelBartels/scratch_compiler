let rec search k = function
    | (k', v)::_ when k = k' -> Some v
    | _::xs -> search k xs
    | [] -> None

let rec update k v = function
    | (k', _)::xs when k = k' -> (k, v)::xs
    | x::xs -> x::(update k v xs)
    | [] -> [(k, v)]
