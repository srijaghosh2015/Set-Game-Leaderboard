let () = Random.self_init ()

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

type t = {
  shape : shape;
  color : color;
  number : int;
  shading : shading;
}

let create_card color shading shape number = { shape; color; number; shading }

let compare_feature f (c1 : t) (c2 : t) (c3 : t) =
  let element1 = f c1 in
  let element2 = f c2 in
  let element3 = f c3 in
  (element1 = element2 && element2 = element3 && element1 = element3)
  || (element1 <> element2 && element2 <> element3 && element1 <> element3)

let is_set (c1 : t) (c2 : t) (c3 : t) : bool =
  compare_feature (fun c -> c.color) c1 c2 c3
  && compare_feature (fun c -> c.shading) c1 c2 c3
  && compare_feature (fun c -> c.shape) c1 c2 c3
  && compare_feature (fun c -> c.number) c1 c2 c3

let get_color c = c.color
let get_shading c = c.shading
let get_shape c = c.shape
let get_num c = c.number

(* All 81 possible cards. *)
let all_cards =
  let colors = [ Red; Purple; Green ] in
  let shapes = [ Oval; Squiggle; Diamond ] in
  let shadings = [ Solid; Striped; Outlined ] in
  let numbers = [ 1; 2; 3 ] in
  List.concat_map
    (fun color ->
      List.concat_map
        (fun shape ->
          List.concat_map
            (fun shading ->
              List.map (fun number -> { color; shape; shading; number }) numbers)
            shadings)
        shapes)
    colors

let rec pick_cards n (all_cards : t list) picked =
  if n = 0 then picked
  else
    let card = List.nth all_cards (Random.int (List.length all_cards)) in
    if List.mem card picked then pick_cards n all_cards picked
    else pick_cards (n - 1) all_cards (card :: picked)

let remove_from_all_cards picked all_cards =
  List.filter (fun element -> not (List.mem element picked)) all_cards

let to_string (c : t) =
  let string_of_color = function
    | Red -> "Red"
    | Purple -> "Purple"
    | Green -> "Green"
  in
  let string_of_shading = function
    | Solid -> "Solid"
    | Striped -> "Striped"
    | Outlined -> "Outlined"
  in
  let string_of_shape = function
    | Oval -> "Oval"
    | Squiggle -> "Squiggle"
    | Diamond -> "Diamond"
  in
  "Color: " ^ string_of_color c.color ^ ", " ^ "Shape: "
  ^ string_of_shape c.shape ^ ", " ^ "Shading: "
  ^ string_of_shading c.shading
  ^ ", " ^ "Number: " ^ string_of_int c.number
