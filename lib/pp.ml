open Format
let rec pp_list f ppf = function
    | x::[] -> fprintf ppf "%a" f x
    | x::xs -> fprintf ppf "%a, %a" f x (pp_list f) xs
    | [] -> ()

let pp_opt f ppf = function
    | Some v -> fprintf ppf "Some (%a)" f v
    | None -> fprintf ppf "None"

let pp_string ppf = fprintf ppf "%s"

let print_pp pp v = pp std_formatter v; print_flush ()
