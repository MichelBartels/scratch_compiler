type 'a t = 'a list
[@@ deriving show]
let contains t = List.exists (fun x -> x = t)
let add t set = if contains t set then set else t::set
let union s = List.fold_left (List.fold_left (fun x y -> add y x)) [] s
let remove v = List.filter (fun v' -> v' <> v)
let from_list xs = List.fold_left (fun x y -> add y x) [] xs
let to_list xs = xs
let empty = []
