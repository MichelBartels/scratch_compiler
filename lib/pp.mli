val pp_list: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
val pp_opt: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit
val pp_string: Format.formatter -> string -> unit
val print_pp: (Format.formatter -> 'a -> unit) -> 'a -> unit
