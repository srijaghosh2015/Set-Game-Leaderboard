type t
(** [t] is the type of set game grid. *)

val create : int -> int -> t
(** [create x y] creates a grid with [x] rows and [y] columns. *)

val get_rows : t -> int
(** [get_rows g] is the number of rows of [g]. *)

val get_cols : t -> int
(** [get_cols g] is the number of columns of [g]. *)

val get_card : t -> int -> int -> Card.t option
(** [get_card g x y ] is the card located at row [x] and column [y] of [g]. *)

val place_cards : t -> Card.t list -> unit
(** [place_cards g c] places cards from the list [c] into the grid [g]. *)

val remove_cards : t -> (int * int) list -> unit
(** [remove_cards g positions] removes the cards at [positions] from the grid
    [g]. *)

val refill_cards : t -> Card.t list -> (int * int) list -> unit
(** [refill_cards g new_cards positions] adds new cards [new_cards] to the empty
    positions [positions] in the grid [g]. *)

val to_list : t -> Card.t list
(** [to_list g] converts grid [g] to a list of cards. *)

val set_exists : t -> bool
(** [set_exists g] is whether a valid 3-card set exists in [g]. *)

val find_set : t -> Card.t list
(** [find_set g] is the list of cards in the set of the grid [g]. Empty list if
    there is no set in the grid. *)

val cheat : t -> string
(** [cheat g] is the string representation of all the cards in the set of grid
    [g]. *)

val color_hint : t -> string
(** [color_hint g] is the string of whether the colors in the set of [g] are the
    same or different. *)

val shading_hint : t -> string
(** [shading_hint g] is the string of whether the shadings in the set of [g] are
    the same or different. *)

val shape_hint : t -> string
(** [shape_hint g] is the string of whether the shapes in the set of [g] are the
    same or different. *)

val num_hint : t -> string
(** [num_hint g] is the string of whether the colors in the set of [g] are the
    same or different. *)

val card_hint : t -> int -> string
(** [card_hint g n] is the string of one of the features of one card in the set
    of [g], based on [n] from 0 to 7 . *)

val hint : t -> string
(** [hint g] is the string of a randomly chosen hint for grid [g]. *)
