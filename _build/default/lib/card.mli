type color =
  | Red
  | Purple
  | Green

type shading =
  | Solid
  | Striped
  | Outlined

type shape =
  | Oval
  | Squiggle
  | Diamond

type t
(** [t] is the type of the card. *)

val create_card : color -> shading -> shape -> int -> t
(** [create_card color shading shape number] creates a card with color [color],
    shading [shading], shape [shape], and number [number]. *)

val compare_feature : (t -> 'b) -> t -> t -> t -> bool
(** [compare_feature f c1 c2 c3] is whether or not the cards [c1], [c2], and
    [c3] have the same features, using function [f]. *)

val is_set : t -> t -> t -> bool
(** [is_set c1 c2 c3] is whether or not [c1], [c2], and [c3] are a set. *)

val get_color : t -> color
(** [get_color c] is the color for [c]. *)

val get_shading : t -> shading
(** [get_shad c] is the shading for [c]. *)

val get_shape : t -> shape
(** [get_shape c] is the shape for [c]. *)

val get_num : t -> int
(** [get_num c] is the number for [c]. *)

val all_cards : t list
(** [all_cards] is a list of all 81 possible cards. *)

val pick_cards : int -> t list -> t list -> t list
(** [pick_cards n cards picked] picks [n] cards from the list of cards [cards]
    into the list [picked]. *)

val remove_from_all_cards : t list -> t list -> t list
(** [remove_from_all_cards picked all_cards] removes elements from [all_cards]
    that are present in [picked]. *)

val to_string : t -> string
(** [to_string c] is the string representation of [c]. *)
