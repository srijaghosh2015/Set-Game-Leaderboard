open Cs3110_final_project
open Card
open Setgame
open Parse
open OUnit2

let assert_no_duplicates lst =
  let table = Hashtbl.create 81 in
  List.iter
    (fun card ->
      assert_bool "duplicate card" (not (Hashtbl.mem table card));
      Hashtbl.add table card ())
    lst

let tests =
  [
    ("no duplicates in all cards" >:: fun _ -> assert_no_duplicates all_cards);
    ( "no duplicates in picked cards" >:: fun _ ->
      assert_no_duplicates (pick_cards 30 all_cards []) );
    ( "different numbers" >:: fun _ ->
      assert_bool "three cards with different numbers is a set"
        (is_set
           (create_card Red Solid Oval 2)
           (create_card Red Solid Oval 1)
           (create_card Red Solid Oval 3)) );
    ( "all different" >:: fun _ ->
      assert_bool "three cards with all different properties is a set"
        (is_set
           (create_card Red Solid Oval 2)
           (create_card Green Striped Squiggle 1)
           (create_card Purple Outlined Diamond 3)) );
    ( "different colors, shading, and numbers" >:: fun _ ->
      assert_bool "different colors, shading, and numbers is a set"
        (is_set
           (create_card Red Solid Squiggle 2)
           (create_card Green Striped Squiggle 1)
           (create_card Purple Outlined Squiggle 3)) );
    ( "all same number" >:: fun _ ->
      assert_bool
        "three cards with same number but all other different properties is a \
         set"
        (is_set
           (create_card Red Solid Oval 2)
           (create_card Green Striped Squiggle 2)
           (create_card Purple Outlined Diamond 2)) );
    ( "all same number and same shape" >:: fun _ ->
      assert_bool "three cards with all the same number and same shape is a set"
        (is_set
           (create_card Red Solid Oval 2)
           (create_card Green Striped Oval 2)
           (create_card Purple Outlined Oval 2)) );
    ( "different color" >:: fun _ ->
      assert_bool "three cards with different colors is a set"
        (is_set
           (create_card Red Solid Oval 2)
           (create_card Green Solid Oval 1)
           (create_card Purple Solid Oval 3)) );
    ( "all elements same" >:: fun _ ->
      assert_bool
        "three cards with all elements the same are a set (note that this \
         cannot occur in the game due to a lack of duplicates)"
        (is_set
           (create_card Red Solid Oval 2)
           (create_card Red Solid Oval 2)
           (create_card Red Solid Oval 2)) );
    ( "different shape" >:: fun _ ->
      assert_bool
        "three cards with different shapes but all other properties the same \
         are a set"
        (is_set
           (create_card Red Solid Oval 2)
           (create_card Red Solid Squiggle 2)
           (create_card Red Solid Diamond 2)) );
    ( "different shape and number" >:: fun _ ->
      assert_bool
        "three cards with different shapes and nunmbers but all other \
         properties the same are a set"
        (is_set
           (create_card Red Solid Oval 1)
           (create_card Red Solid Squiggle 2)
           (create_card Red Solid Diamond 3)) );
    ( "different shading" >:: fun _ ->
      assert_bool
        "three cards with different shading but all other properties the same \
         are a set"
        (is_set
           (create_card Red Solid Oval 1)
           (create_card Red Outlined Oval 1)
           (create_card Red Striped Oval 1)) );
    ( "different shading and number" >:: fun _ ->
      assert_bool
        "three cards with different shading and number of elements, but all \
         other properties the same are a set"
        (is_set
           (create_card Red Solid Oval 1)
           (create_card Red Outlined Oval 2)
           (create_card Red Striped Oval 3)) );
    ( "different shading and shape" >:: fun _ ->
      assert_bool
        "three cards with different shading and shape but all other properties \
         the same are a set"
        (is_set
           (create_card Red Solid Oval 1)
           (create_card Red Outlined Diamond 1)
           (create_card Red Striped Squiggle 1)) );
    ( "different shading and color" >:: fun _ ->
      assert_bool
        "three cards with different shading and color but all other properties \
         the same are a set"
        (is_set
           (create_card Red Solid Oval 1)
           (create_card Green Outlined Oval 1)
           (create_card Purple Striped Oval 1)) );
    ( "all same color" >:: fun _ ->
      assert_bool
        "three cards with the same color but all other properties different \
         for each card are a set"
        (is_set
           (create_card Red Solid Oval 1)
           (create_card Red Outlined Diamond 2)
           (create_card Red Striped Squiggle 3)) );
    ( "different color and number" >:: fun _ ->
      assert_bool
        "three cards with different colors and number of elements, but all \
         other properties the same are a set"
        (is_set
           (create_card Red Solid Oval 1)
           (create_card Green Solid Oval 2)
           (create_card Purple Solid Oval 3)) );
    ( "different color and shape" >:: fun _ ->
      assert_bool
        "three cards with different colors and shape, but all other properties \
         the same are a set"
        (is_set
           (create_card Red Solid Oval 1)
           (create_card Green Solid Squiggle 1)
           (create_card Purple Solid Diamond 1)) );
    ( "same shading" >:: fun _ ->
      assert_bool
        "three cards with the same shading, but all other properties different \
         for each card are a set"
        (is_set
           (create_card Red Solid Oval 1)
           (create_card Green Solid Squiggle 1)
           (create_card Purple Solid Diamond 1)) );
    ( "same two colors" >:: fun _ ->
      assert_bool "two out of the three cards have the same color is not a set"
        (not
           (is_set
              (create_card Red Solid Oval 2)
              (create_card Green Solid Oval 1)
              (create_card Red Solid Oval 3))) );
    ( "same two colors and same two shapes" >:: fun _ ->
      assert_bool "two cards that have the same color and shape is not a set"
        (not
           (is_set
              (create_card Red Solid Oval 2)
              (create_card Green Solid Squiggle 1)
              (create_card Red Solid Squiggle 3))) );
    ( "same two colors, same two shading, same two shape" >:: fun _ ->
      assert_bool "three cards that are not a set"
        (not
           (is_set
              (create_card Red Solid Oval 2)
              (create_card Green Outlined Squiggle 1)
              (create_card Red Solid Squiggle 3))) );
    ( "same two shape" >:: fun _ ->
      assert_bool "three cards that are not a set"
        (not
           (is_set
              (create_card Red Solid Diamond 2)
              (create_card Green Striped Squiggle 1)
              (create_card Purple Outlined Diamond 3))) );
    ( "same two numbers" >:: fun _ ->
      assert_bool "three cards that are not a set"
        (not
           (is_set
              (create_card Red Solid Diamond 2)
              (create_card Green Striped Squiggle 2)
              (create_card Purple Outlined Oval 3))) );
    ( "same two numbers, same two colors, same two shape" >:: fun _ ->
      assert_bool "three cards that are not a set"
        (not
           (is_set
              (create_card Red Solid Diamond 2)
              (create_card Green Striped Squiggle 2)
              (create_card Red Outlined Diamond 3))) );
    ( "same two numbers, same two shape" >:: fun _ ->
      assert_bool "three cards that are not a set"
        (not
           (is_set
              (create_card Red Solid Diamond 2)
              (create_card Red Striped Squiggle 2)
              (create_card Red Outlined Diamond 3))) );
    ( "same two numbers, same two shape" >:: fun _ ->
      assert_bool "three cards that are not a set"
        (not
           (is_set
              (create_card Red Solid Diamond 1)
              (create_card Red Striped Squiggle 1)
              (create_card Red Outlined Diamond 3))) );
    ( "same two colors, same two shape" >:: fun _ ->
      assert_bool "three cards that are not a set"
        (not
           (is_set
              (create_card Red Solid Diamond 1)
              (create_card Red Striped Squiggle 2)
              (create_card Green Outlined Diamond 3))) );
    ( "same two shading, same two shape" >:: fun _ ->
      assert_bool "three cards that are not a set"
        (not
           (is_set
              (create_card Red Striped Diamond 1)
              (create_card Purple Striped Squiggle 2)
              (create_card Green Outlined Diamond 3))) );
    ( "same two shading" >:: fun _ ->
      assert_bool "three cards that are not a set"
        (not
           (is_set
              (create_card Red Striped Diamond 1)
              (create_card Purple Striped Squiggle 2)
              (create_card Green Outlined Oval 3))) );
    ( "same two shading, same two number" >:: fun _ ->
      assert_bool "three cards that are not a set"
        (not
           (is_set
              (create_card Red Striped Diamond 1)
              (create_card Purple Striped Squiggle 2)
              (create_card Green Outlined Oval 2))) );
    ( "same two shading, same two number, same two shape" >:: fun _ ->
      assert_bool "three cards that are not a set"
        (not
           (is_set
              (create_card Red Striped Diamond 1)
              (create_card Purple Striped Diamond 2)
              (create_card Green Outlined Oval 2))) );
    ( "same two color, same two number" >:: fun _ ->
      assert_bool "three cards that are not a set"
        (not
           (is_set
              (create_card Red Striped Diamond 1)
              (create_card Red Solid Diamond 1)
              (create_card Green Outlined Oval 3))) );
    ( "same two color, same two shading" >:: fun _ ->
      assert_bool "three cards that are not a set"
        (not
           (is_set
              (create_card Red Striped Diamond 1)
              (create_card Red Striped Squiggle 2)
              (create_card Green Outlined Oval 3))) );
    ( "same two color, same two shading, same two number" >:: fun _ ->
      assert_bool "three cards that are not a set"
        (not
           (is_set
              (create_card Red Solid Diamond 1)
              (create_card Red Solid Squiggle 2)
              (create_card Green Outlined Oval 2))) );
    ( "same two everything" >:: fun _ ->
      assert_bool "three cards that are not a set"
        (not
           (is_set
              (create_card Red Solid Diamond 1)
              (create_card Red Solid Oval 2)
              (create_card Green Outlined Oval 2))) );
    ( "compare_feature all color red" >:: fun _ ->
      assert_bool "three cards are all red"
        (compare_feature get_color
           (create_card Red Solid Oval 2)
           (create_card Red Solid Oval 1)
           (create_card Red Solid Oval 3)) );
    ( "compare_feature all different colors" >:: fun _ ->
      assert_bool "three cards are all different colors"
        (compare_feature get_color
           (create_card Red Solid Oval 2)
           (create_card Green Solid Oval 1)
           (create_card Purple Solid Oval 3)) );
    ( "compare_feature two same color" >:: fun _ ->
      assert_bool "two cards have the same color"
        (not
           (compare_feature get_color
              (create_card Red Solid Oval 2)
              (create_card Green Solid Oval 1)
              (create_card Green Solid Oval 3))) );
    ( "compare_feature all same number" >:: fun _ ->
      assert_bool "three cards are all same number"
        (compare_feature get_num
           (create_card Red Solid Oval 2)
           (create_card Green Solid Oval 2)
           (create_card Green Solid Oval 2)) );
    ( "compare_feature all different number" >:: fun _ ->
      assert_bool "three cards are all different number"
        (compare_feature get_num
           (create_card Red Solid Oval 1)
           (create_card Green Solid Oval 2)
           (create_card Green Solid Oval 3)) );
    ( "compare_feature two same number" >:: fun _ ->
      assert_bool "two cards have the same number"
        (not
           (compare_feature get_num
              (create_card Red Solid Oval 2)
              (create_card Green Solid Oval 2)
              (create_card Green Solid Oval 3))) );
    ( "compare_feature all same shading" >:: fun _ ->
      assert_bool "three cards are all same shading"
        (compare_feature get_shading
           (create_card Red Solid Oval 1)
           (create_card Green Solid Squiggle 2)
           (create_card Purple Solid Diamond 3)) );
    ( "compare_feature all different shading" >:: fun _ ->
      assert_bool "three cards are all different shading"
        (compare_feature get_shading
           (create_card Red Solid Oval 1)
           (create_card Green Striped Squiggle 2)
           (create_card Purple Outlined Diamond 3)) );
    ( "compare_feature two same shading" >:: fun _ ->
      assert_bool "two cards have the same shading"
        (not
           (compare_feature get_shading
              (create_card Red Solid Oval 1)
              (create_card Green Solid Squiggle 2)
              (create_card Purple Outlined Diamond 3))) );
    ( "compare_feature all same shape" >:: fun _ ->
      assert_bool "three cards are all same shape"
        (compare_feature get_shape
           (create_card Red Solid Oval 1)
           (create_card Green Striped Oval 2)
           (create_card Purple Outlined Oval 3)) );
    ( "compare_feature all different shape" >:: fun _ ->
      assert_bool "three cards are all different shape"
        (compare_feature get_shape
           (create_card Red Solid Oval 1)
           (create_card Green Striped Squiggle 2)
           (create_card Purple Outlined Diamond 3)) );
    ( "compare_feature two same shape" >:: fun _ ->
      assert_bool "two cards have the same shape"
        (not
           (compare_feature get_shape
              (create_card Red Solid Oval 1)
              (create_card Green Striped Oval 2)
              (create_card Purple Outlined Diamond 3))) );
  ]

let parse_tests =
  [
    ( "correct input with all A" >:: fun _ ->
      assert_equal [ (0, 0); (0, 1); (0, 2) ] (parse_input "A0, A1, A2") );
    ( "correct input with all 0" >:: fun _ ->
      assert_equal [ (0, 0); (1, 0); (2, 0) ] (parse_input "A0, B0, C0") );
    ( "all different inputs" >:: fun _ ->
      assert_equal [ (1, 0); (2, 1); (3, 2) ] (parse_input "B0, C1, D2") );
    ( "all different inputs 2" >:: fun _ ->
      assert_equal [ (1, 0); (1, 2); (2, 2) ] (parse_input "B0, B2, C2") );
    ( "all different inputs 3" >:: fun _ ->
      assert_equal [ (0, 0); (3, 2); (2, 2) ] (parse_input "A0, D2, C2") );
    ( "more spacing in middle" >:: fun _ ->
      assert_bool "incorrect input"
        (try
           let _ = parse_input "A 0, A1, A2" in
           false
         with Invalid_argument _ -> true) );
    ( "no letters" >:: fun _ ->
      assert_bool "incorrect input"
        (try
           let _ = parse_input "00, 02, A2" in
           false
         with Invalid_argument _ -> true) );
    ( "no numbers" >:: fun _ ->
      assert_bool "incorrect input"
        (try
           let _ = parse_input "AA, BA, CA" in
           false
         with Invalid_argument _ -> true) );
    ( "empty string" >:: fun _ ->
      assert_bool "incorrect input"
        (try
           let _ = parse_input "" in
           false
         with Invalid_argument _ -> true) );
    ( "out of bounds letter" >:: fun _ ->
      assert_bool "incorrect input"
        (try
           let _ = parse_input "F0, A1, A2" in
           false
         with Invalid_argument _ -> true) );
    ( "out of bounds number" >:: fun _ ->
      assert_bool "incorrect input"
        (try
           let _ = parse_input "A0, A1, A5" in
           false
         with Invalid_argument _ -> true) );
    ( "too many inputs" >:: fun _ ->
      assert_bool "incorrect input"
        (try
           let _ = parse_input "A0, A1, A2, A4" in
           false
         with Invalid_argument _ -> true) );
    ( "one duplicate input" >:: fun _ ->
      assert_bool "incorrect input"
        (try
           let _ = parse_input "A0, A0, A0" in
           false
         with Invalid_argument _ -> true) );
    ( "two duplicate inputs" >:: fun _ ->
      assert_bool "incorrect input"
        (try
           let _ = parse_input "A0, A0, C1" in
           false
         with Invalid_argument _ -> true) );
    ( "too few inputs (two)" >:: fun _ ->
      assert_bool "incorrect input"
        (try
           let _ = parse_input "A0, A1" in
           false
         with Invalid_argument _ -> true) );
    ( "too few inputs (one)" >:: fun _ ->
      assert_bool "incorrect input"
        (try
           let _ = parse_input "A0" in
           false
         with Invalid_argument _ -> true) );
    ( "no commas" >:: fun _ ->
      assert_bool "incorrect input"
        (try
           let _ = parse_input "A0 A1 A2" in
           false
         with Invalid_argument _ -> true) );
    ( "lowercase letters" >:: fun _ ->
      assert_bool "incorrect input"
        (try
           let _ = parse_input "a0, b2, b1" in
           false
         with Invalid_argument _ -> true) );
    ( "order of letters and numbers swapped" >:: fun _ ->
      assert_bool "incorrect input"
        (try
           let _ = parse_input "2A, 1B, 0C" in
           false
         with Invalid_argument _ -> true) );
    ( "too many characters" >:: fun _ ->
      assert_bool "incorrect input"
        (try
           let _ = parse_input "A00, B0, C0" in
           false
         with Invalid_argument _ -> true) );
    ( "one input correct but not others" >:: fun _ ->
      assert_bool "incorrect input"
        (try
           let _ = parse_input "D2, aba, b7" in
           false
         with Invalid_argument _ -> true) );
    ( "no spaces in between" >:: fun _ ->
      assert_bool "incorrect input"
        (try
           let _ = parse_input "D2,A1,A3" in
           false
         with Invalid_argument _ -> true) );
    ( "other characters" >:: fun _ ->
      assert_bool "incorrect input"
        (try
           let _ = parse_input "D2, A!, A3" in
           false
         with Invalid_argument _ -> true) );
  ]

let card_tests =
  [
    ( "get color" >:: fun _ ->
      assert_equal Red (get_color (create_card Red Solid Oval 2)) );
    ( "get shading" >:: fun _ ->
      assert_equal Solid (get_shading (create_card Red Solid Oval 2)) );
    ( "get shape" >:: fun _ ->
      assert_equal Oval (get_shape (create_card Red Solid Oval 2)) );
    ( "get num" >:: fun _ ->
      assert_equal 2 (get_num (create_card Red Solid Oval 2)) );
    ( "pick 12 cards" >:: fun _ ->
      let cards = pick_cards 12 all_cards [] in
      assert_equal (List.length cards) 12 );
    ( "remove cards from card list" >:: fun _ ->
      let picked =
        [ create_card Red Solid Oval 2; create_card Green Solid Oval 2 ]
      in
      let all_cards =
        [
          create_card Red Solid Oval 2;
          create_card Green Solid Oval 2;
          create_card Purple Solid Oval 2;
        ]
      in
      assert_equal
        [ create_card Purple Solid Oval 2 ]
        (remove_from_all_cards picked all_cards) );
    ( "to string" >:: fun _ ->
      let card = create_card Red Solid Oval 2 in
      assert_equal "Color: Red, Shape: Oval, Shading: Solid, Number: 2"
        (to_string card) );
  ]

let setgame_tests =
  [
    ( "get rows" >:: fun _ ->
      let grid = create 4 3 in
      assert_equal 4 (get_rows grid) );
    ( "get columns" >:: fun _ ->
      let grid = create 4 3 in
      assert_equal 3 (get_cols grid) );
    ( "place 1 card into 1 cell grid" >:: fun _ ->
      let card = [ create_card Red Solid Oval 2 ] in
      let grid = create 1 1 in
      let () = place_cards grid card in
      assert_equal [ create_card Red Solid Oval 2 ] (to_list grid) );
    ( "place 1 card into 12 cell grid " >:: fun _ ->
      assert_bool "Not enough cards."
        (try
           let card = [ create_card Red Solid Oval 2 ] in
           let grid = create 4 3 in
           let () = place_cards grid card in
           false
         with Failure _ -> true) );
    ( "remove one card from grid" >:: fun _ ->
      let card = [ create_card Red Solid Oval 2 ] in
      let grid = create 1 1 in
      let () = place_cards grid card in
      let () = remove_cards grid [ (0, 0) ] in
      assert_equal (create 1 1) grid );
    ( "remove two cards from grid" >:: fun _ ->
      let card =
        [ create_card Red Solid Oval 2; create_card Green Striped Squiggle 3 ]
      in
      let grid = create 2 1 in
      let () = place_cards grid card in
      let () = remove_cards grid [ (0, 0); (1, 0) ] in
      assert_equal (create 2 1) grid );
    ( "remove three cards from grid" >:: fun _ ->
      let card =
        [
          create_card Red Solid Oval 2;
          create_card Green Striped Squiggle 3;
          create_card Red Striped Squiggle 3;
        ]
      in
      let grid = create 3 1 in
      let () = place_cards grid card in
      let () = remove_cards grid [ (0, 0); (1, 0); (2, 0) ] in
      assert_equal (create 3 1) grid );
    ( "remove card from grid at invalid position " >:: fun _ ->
      assert_bool "Position invalid."
        (try
           let card = [ create_card Red Solid Oval 2 ] in
           let grid = create 1 1 in
           let () = place_cards grid card in
           let () = remove_cards grid [ (2, 2) ] in
           false
         with Invalid_argument _ -> true) );
    ( "remove card from empty grid" >:: fun _ ->
      assert_bool "Empty grid."
        (try
           let grid = create 1 1 in
           let () = remove_cards grid [ (2, 2) ] in
           false
         with Invalid_argument _ -> true) );
    ( "refill cards" >:: fun _ ->
      let card = [ create_card Red Solid Oval 2 ] in
      let grid = create 1 1 in
      let () = refill_cards grid card [ (0, 0) ] in
      let grid2 = create 1 1 in
      let () = place_cards grid2 card in
      assert_equal grid grid2 );
    ( "refill cards at invalid position " >:: fun _ ->
      assert_bool "Position invalid."
        (try
           let card = [ create_card Red Solid Oval 2 ] in
           let grid = create 1 1 in
           let () = refill_cards grid card [ (1, 1) ] in
           let grid2 = create 1 1 in
           let () = place_cards grid2 card in
           false
         with Invalid_argument _ -> true) );
    ( "set exists - different numbers" >:: fun _ ->
      let card1 = create_card Red Solid Oval 2 in
      let card2 = create_card Red Solid Oval 1 in
      let card3 = create_card Red Solid Oval 3 in
      let card4 = create_card Purple Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_bool "three cards with different numbers is a set"
        (set_exists grid) );
    ( "set exists - all different" >:: fun _ ->
      let card1 = create_card Red Solid Oval 2 in
      let card2 = create_card Green Striped Squiggle 1 in
      let card3 = create_card Purple Outlined Diamond 3 in
      let card4 = create_card Purple Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_bool "three cards with all different properties is a set"
        (set_exists grid) );
    ( "set exists - different colors, shading, and numbers" >:: fun _ ->
      let card1 = create_card Red Solid Squiggle 2 in
      let card2 = create_card Green Striped Squiggle 1 in
      let card3 = create_card Purple Outlined Squiggle 3 in
      let card4 = create_card Purple Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_bool "different colors, shading, and numbers is a set"
        (set_exists grid) );
    ( "set exists - all same number" >:: fun _ ->
      let card1 = create_card Red Solid Oval 2 in
      let card2 = create_card Green Striped Squiggle 2 in
      let card3 = create_card Purple Outlined Diamond 2 in
      let card4 = create_card Purple Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_bool
        "three cards with same number but all other different properties is a \
         set"
        (set_exists grid) );
    ( "set exists - all same number and same shape" >:: fun _ ->
      let card1 = create_card Red Solid Oval 2 in
      let card2 = create_card Green Striped Oval 2 in
      let card3 = create_card Purple Outlined Oval 2 in
      let card4 = create_card Purple Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_bool "three cards with all the same number and same shape is a set"
        (set_exists grid) );
    ( "set exists - all same shading" >:: fun _ ->
      let card1 = create_card Red Solid Oval 2 in
      let card2 = create_card Green Solid Squiggle 1 in
      let card3 = create_card Purple Solid Diamond 3 in
      let card4 = create_card Purple Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_bool "three cards with all the same shading is a set"
        (set_exists grid) );
    ( "set exists - all same shape" >:: fun _ ->
      let card1 = create_card Red Solid Oval 2 in
      let card2 = create_card Green Striped Oval 1 in
      let card3 = create_card Purple Outlined Oval 3 in
      let card4 = create_card Purple Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_bool "three cards with all the same shape is a set"
        (set_exists grid) );
    ( "set exists - different color" >:: fun _ ->
      let card1 = create_card Red Solid Oval 2 in
      let card2 = create_card Green Solid Oval 1 in
      let card3 = create_card Purple Solid Oval 3 in
      let card4 = create_card Purple Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_bool "three cards with different colors is a set" (set_exists grid)
    );
    ( "set exists - different shape" >:: fun _ ->
      let card1 = create_card Red Solid Oval 2 in
      let card2 = create_card Red Solid Squiggle 2 in
      let card3 = create_card Red Solid Diamond 2 in
      let card4 = create_card Purple Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_bool "three cards with different shapes is a set" (set_exists grid)
    );
    ( "set exists - different shading" >:: fun _ ->
      let card1 = create_card Red Solid Oval 2 in
      let card2 = create_card Red Striped Oval 2 in
      let card3 = create_card Red Outlined Oval 2 in
      let card4 = create_card Purple Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_bool "three cards with different shadings is a set"
        (set_exists grid) );
    ( "set does not exist - two same color" >:: fun _ ->
      let card1 = create_card Red Solid Oval 2 in
      let card2 = create_card Green Solid Oval 1 in
      let card3 = create_card Red Solid Oval 3 in
      let card4 = create_card Purple Solid Diamond 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_bool "no set" (not (set_exists grid)) );
    ( "set does not exist - two same shading" >:: fun _ ->
      let card1 = create_card Red Solid Oval 2 in
      let card2 = create_card Green Solid Oval 1 in
      let card3 = create_card Purple Striped Oval 3 in
      let card4 = create_card Purple Solid Diamond 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_bool "no set" (not (set_exists grid)) );
    ( "set does not exist - empty grid" >:: fun _ ->
      let grid = create 2 2 in
      assert_bool "no set" (not (set_exists grid)) );
    ( "set does not exist - 2 cards" >:: fun _ ->
      let card1 = create_card Red Solid Oval 2 in
      let card2 = create_card Green Solid Oval 1 in
      let card3 = create_card Red Solid Oval 3 in
      let card4 = create_card Purple Solid Diamond 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      let () = remove_cards grid [ (0, 0); (0, 1) ] in
      assert_bool "no set" (not (set_exists grid)) );
    ( "set does not exist - 1 card" >:: fun _ ->
      let card1 = create_card Red Solid Oval 2 in
      let card2 = create_card Green Solid Oval 1 in
      let card3 = create_card Red Solid Oval 3 in
      let card4 = create_card Purple Solid Diamond 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      let () = remove_cards grid [ (0, 0); (0, 1); (1, 0) ] in
      assert_bool "no set" (not (set_exists grid)) );
    ( "cheat - set exists" >:: fun _ ->
      let card1 = create_card Red Solid Oval 1 in
      let card2 = create_card Red Solid Oval 2 in
      let card3 = create_card Red Solid Oval 3 in
      let card4 = create_card Purple Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_equal
        (to_string card2 ^ ";\n" ^ to_string card3 ^ ";\n" ^ to_string card1)
        (cheat grid) );
    ( "cheat - set does not exist" >:: fun _ ->
      let card1 = create_card Red Solid Oval 2 in
      let card2 = create_card Green Solid Oval 1 in
      let card3 = create_card Red Solid Oval 3 in
      let card4 = create_card Purple Outlined Diamond 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_equal "There is no set. Enter 'No set' to redraw cards."
        (cheat grid) );
    ( "color hint - same" >:: fun _ ->
      let card1 = create_card Red Solid Oval 1 in
      let card2 = create_card Red Solid Oval 2 in
      let card3 = create_card Red Solid Oval 3 in
      let card4 = create_card Purple Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_equal "The cards in the set all have the same color."
        (color_hint grid) );
    ( "color hint - different" >:: fun _ ->
      let card1 = create_card Red Solid Oval 1 in
      let card2 = create_card Purple Solid Oval 1 in
      let card3 = create_card Green Solid Oval 1 in
      let card4 = create_card Purple Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_equal "The cards in the set all have different colors."
        (color_hint grid) );
    ( "color hint - no set" >:: fun _ ->
      let card1 = create_card Red Solid Oval 2 in
      let card2 = create_card Green Solid Oval 1 in
      let card3 = create_card Red Solid Oval 3 in
      let card4 = create_card Purple Outlined Diamond 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_equal "There is no set. Enter 'No set' to redraw cards."
        (color_hint grid) );
    ( "shading hint - same" >:: fun _ ->
      let card1 = create_card Red Solid Oval 1 in
      let card2 = create_card Purple Solid Oval 1 in
      let card3 = create_card Green Solid Oval 1 in
      let card4 = create_card Purple Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_equal "The cards in the set all have the same shading."
        (shading_hint grid) );
    ( "shading hint - different" >:: fun _ ->
      let card1 = create_card Red Solid Oval 1 in
      let card2 = create_card Red Outlined Oval 1 in
      let card3 = create_card Red Striped Oval 1 in
      let card4 = create_card Red Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_equal "The cards in the set all have different shading."
        (shading_hint grid) );
    ( "shading hint - no set" >:: fun _ ->
      let card1 = create_card Red Solid Oval 2 in
      let card2 = create_card Green Solid Oval 1 in
      let card3 = create_card Red Solid Oval 3 in
      let card4 = create_card Purple Outlined Diamond 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_equal "There is no set. Enter 'No set' to redraw cards."
        (shading_hint grid) );
    ( "shape hint - same" >:: fun _ ->
      let card1 = create_card Red Solid Oval 1 in
      let card2 = create_card Red Outlined Oval 1 in
      let card3 = create_card Red Striped Oval 1 in
      let card4 = create_card Red Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_equal "The cards in the set all have the same shape."
        (shape_hint grid) );
    ( "shape hint - different" >:: fun _ ->
      let card1 = create_card Red Solid Oval 1 in
      let card2 = create_card Red Solid Squiggle 1 in
      let card3 = create_card Red Solid Diamond 1 in
      let card4 = create_card Red Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_equal "The cards in the set all have different shapes."
        (shape_hint grid) );
    ( "shape hint - no set" >:: fun _ ->
      let card1 = create_card Red Solid Oval 2 in
      let card2 = create_card Green Solid Oval 1 in
      let card3 = create_card Red Solid Oval 3 in
      let card4 = create_card Purple Outlined Diamond 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_equal "There is no set. Enter 'No set' to redraw cards."
        (shape_hint grid) );
    ( "number hint - same" >:: fun _ ->
      let card1 = create_card Red Solid Oval 1 in
      let card2 = create_card Red Solid Squiggle 1 in
      let card3 = create_card Red Solid Diamond 1 in
      let card4 = create_card Red Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_equal "The cards in the set all have the same number."
        (num_hint grid) );
    ( "number hint - different" >:: fun _ ->
      let card1 = create_card Red Solid Oval 1 in
      let card2 = create_card Red Solid Squiggle 2 in
      let card3 = create_card Red Solid Diamond 3 in
      let card4 = create_card Red Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_equal "The cards in the set all have different numbers."
        (num_hint grid) );
    ( "number hint - no set" >:: fun _ ->
      let card1 = create_card Red Solid Oval 2 in
      let card2 = create_card Green Solid Oval 1 in
      let card3 = create_card Red Solid Oval 3 in
      let card4 = create_card Purple Outlined Diamond 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_equal "There is no set. Enter 'No set' to redraw cards."
        (num_hint grid) );
    ( "card hint - color" >:: fun _ ->
      let card1 = create_card Red Solid Oval 1 in
      let card2 = create_card Red Outlined Oval 1 in
      let card3 = create_card Red Striped Oval 1 in
      let card4 = create_card Red Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_equal "One of the cards in the set has Color: Red"
        (card_hint grid 0) );
    ( "card hint - shape" >:: fun _ ->
      let card1 = create_card Red Solid Oval 1 in
      let card2 = create_card Red Outlined Oval 1 in
      let card3 = create_card Red Striped Oval 1 in
      let card4 = create_card Red Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_equal "One of the cards in the set has Shape: Oval"
        (card_hint grid 1) );
    ( "card hint - shading" >:: fun _ ->
      let card1 = create_card Red Solid Oval 1 in
      let card2 = create_card Red Outlined Oval 1 in
      let card3 = create_card Red Striped Oval 1 in
      let card4 = create_card Green Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_equal "One of the cards in the set has Shading: Outlined"
        (card_hint grid 2) );
    ( "card hint - number" >:: fun _ ->
      let card1 = create_card Red Solid Oval 1 in
      let card2 = create_card Red Outlined Oval 1 in
      let card3 = create_card Red Striped Oval 1 in
      let card4 = create_card Red Solid Oval 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_equal "One of the cards in the set has Number: 1"
        (card_hint grid 3) );
    ( "card hint - no set" >:: fun _ ->
      let card1 = create_card Red Solid Oval 2 in
      let card2 = create_card Green Solid Oval 1 in
      let card3 = create_card Red Solid Oval 3 in
      let card4 = create_card Purple Outlined Diamond 2 in
      let grid = create 2 2 in
      let () = place_cards grid [ card1; card2; card3; card4 ] in
      assert_equal "There is no set. Enter 'No set' to redraw cards."
        (card_hint grid 3) );
  ]

let test_suite =
  "test suite"
  >::: List.flatten [ tests; parse_tests; card_tests; setgame_tests ]

let _ = run_test_tt_main test_suite
