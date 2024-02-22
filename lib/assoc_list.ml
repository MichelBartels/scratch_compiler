let rec search_assoc_list k = function
    | (k', v)::_ when k = k' -> Some v
    | _::xs -> search_assoc_list k xs
    | [] -> None

let rec update_assoc_list k v = function
    | (k', _)::xs when k = k' -> (k, v)::xs
    | x::xs -> x::(update_assoc_list k v xs)
    | [] -> [(k, v)]
