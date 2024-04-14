type 'a t
val contains: 'a -> 'a t -> bool
val add: 'a -> 'a t -> 'a t
val union: 'a t list -> 'a t
val remove: 'a -> 'a t -> 'a t
val from_list: 'a list -> 'a t
val to_list: 'a t -> 'a list
val pp: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val empty: 'a t
