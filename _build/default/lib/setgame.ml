type t = Card.t option array array

let create row col : t = Array.make_matrix row col None
let get_rows (g : t) = Array.length g
let get_cols (g : t) = Array.length g.(0)

let get_card (g : t) x y =
  try g.(x).(y)
  with Invalid_argument _ ->
    print_endline
      "At least one of your cards is not a valid card. Please make sure the \
       cards you select are all displayed on the grid.";
    None

let place_cards (g : t) c =
  let n = ref 0 in
  for y = 0 to get_cols g - 1 do
    for x = 0 to get_rows g - 1 do
      g.(x).(y) <- Some (List.nth c !n);
      n := !n + 1
    done
  done

let remove_cards (g : t) positions =
  List.iter (fun (x, y) -> g.(x).(y) <- None) positions

let refill_cards (g : t) new_cards positions =
  List.iter2 (fun (x, y) card -> g.(x).(y) <- Some card) positions new_cards

let to_list (g : t) =
  let rows = get_rows g in
  let cols = get_cols g in
  let rec row_to_list row col acc =
    if col >= cols then acc
    else
      match get_card g row col with
      | Some card -> row_to_list row (col + 1) (card :: acc)
      | None -> row_to_list row (col + 1) acc
  in
  let rec grid_to_list row acc =
    if row >= rows then acc
    else
      let row_list = row_to_list row 0 [] in
      grid_to_list (row + 1) (row_list @ acc)
  in
  grid_to_list 0 []

let set_exists (g : t) =
  let cards = to_list g in
  let len = List.length cards in
  let rec iter_combinations_outermost idx1 =
    if idx1 >= len - 2 then false
    else
      let c1 = List.nth cards idx1 in
      let rec iter_combinations_middle idx2 =
        if idx2 >= len - 1 then iter_combinations_outermost (idx1 + 1)
        else
          let c2 = List.nth cards idx2 in
          let rec iter_combinations_inner idx3 =
            if idx3 >= len then iter_combinations_middle (idx2 + 1)
            else
              let c3 = List.nth cards idx3 in
              if Card.is_set c1 c2 c3 then true
              else iter_combinations_inner (idx3 + 1)
          in
          iter_combinations_inner (idx2 + 1)
      in
      iter_combinations_middle (idx1 + 1)
  in
  iter_combinations_outermost 0

let find_set (g : t) =
  if not (set_exists g) then []
  else
    let cards = to_list g in
    let len = List.length cards in
    let rec iter_combinations_outermost idx1 =
      if idx1 >= len - 2 then []
      else
        let c1 = List.nth cards idx1 in
        let rec iter_combinations_middle idx2 =
          if idx2 >= len - 1 then iter_combinations_outermost (idx1 + 1)
          else
            let c2 = List.nth cards idx2 in
            let rec iter_combinations_inner idx3 =
              if idx3 >= len then iter_combinations_middle (idx2 + 1)
              else
                let c3 = List.nth cards idx3 in
                if Card.is_set c1 c2 c3 then [ c1; c2; c3 ]
                else iter_combinations_inner (idx3 + 1)
            in
            iter_combinations_inner (idx2 + 1)
        in
        iter_combinations_middle (idx1 + 1)
    in
    iter_combinations_outermost 0

let cheat (g : t) =
  let set = find_set g in
  match set with
  | [] -> "There is no set. Enter 'No set' to redraw cards."
  | hd :: tl ->
      Card.to_string hd ^ ";\n"
      ^ Card.to_string (List.hd tl)
      ^ ";\n"
      ^ Card.to_string (List.nth tl 1)

let color_hint (g : t) =
  let setlist = find_set g in
  match setlist with
  | [] -> "There is no set. Enter 'No set' to redraw cards."
  | hd :: tl ->
      let c1 = hd in
      let c2 = List.hd tl in
      if Card.get_color c1 = Card.get_color c2 then
        "The cards in the set all have the same color."
      else "The cards in the set all have different colors."

let shading_hint (g : t) =
  let setlist = find_set g in
  match setlist with
  | [] -> "There is no set. Enter 'No set' to redraw cards."
  | hd :: tl ->
      let c1 = hd in
      let c2 = List.hd tl in
      if Card.get_shading c1 = Card.get_shading c2 then
        "The cards in the set all have the same shading."
      else "The cards in the set all have different shading."

let shape_hint (g : t) =
  let setlist = find_set g in
  match setlist with
  | [] -> "There is no set. Enter 'No set' to redraw cards."
  | hd :: tl ->
      let c1 = hd in
      let c2 = List.hd tl in
      if Card.get_shape c1 = Card.get_shape c2 then
        "The cards in the set all have the same shape."
      else "The cards in the set all have different shapes."

let num_hint (g : t) =
  let setlist = find_set g in
  match setlist with
  | [] -> "There is no set. Enter 'No set' to redraw cards."
  | hd :: tl ->
      let c1 = hd in
      let c2 = List.hd tl in
      if Card.get_num c1 = Card.get_num c2 then
        "The cards in the set all have the same number."
      else "The cards in the set all have different numbers."

let card_hint (g : t) (n : int) =
  let setlist = find_set g in
  match setlist with
  | [] -> "There is no set. Enter 'No set' to redraw cards."
  | hd :: _ -> (
      let c = hd in
      let features = String.split_on_char ',' (Card.to_string c) in
      match n with
      | 0 -> "One of the cards in the set has " ^ List.hd features
      | 1 -> "One of the cards in the set has" ^ List.nth features 1
      | 2 -> "One of the cards in the set has" ^ List.nth features 2
      | 3 -> "One of the cards in the set has" ^ List.nth features 3
      | _ -> "")

let () = Random.self_init ()

let hint (g : t) =
  let n = Random.int 8 in
  match n with
  | 0 -> color_hint g
  | 1 -> shading_hint g
  | 2 -> shape_hint g
  | 3 -> num_hint g
  | 4 -> card_hint g 0
  | 5 -> card_hint g 1
  | 6 -> card_hint g 2
  | 7 -> card_hint g 3
  | _ -> ""
