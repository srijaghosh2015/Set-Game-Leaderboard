(** @author Razika Rahman (rr772)
    @author Srija Ghosh (sg2293)
    @author Helen Bian (hjb94)
    @author Salena Tang (st977) *)

open Cs3110_final_project
open Card
open Setgame
open Parse

(** [num_spacing card] is a string of spaces to adjust for the number of the
    card [card]. *)
let num_spacing card =
  match get_num card with
  | 1 -> "          "
  | 2 -> "     "
  | _ -> ""

(** [card_rep c] prints the card representation to the console. *)
let card_rep (c : Card.t) =
  let color_code =
    match get_color c with
    | Red -> "\x1b[31m"
    | Green -> "\x1b[92m"
    | Purple -> "\x1b[35m"
  in
  let reset_code = "\x1b[0m" in
  let shad_shape c =
    match (get_shading c, get_shape c) with
    | Solid, Oval -> " (█) "
    | Solid, Squiggle -> " {█} "
    | Solid, Diamond -> " <█> "
    | Striped, Oval -> " (≡) "
    | Striped, Squiggle -> " {≡} "
    | Striped, Diamond -> " <≡> "
    | Outlined, Oval -> " ( ) "
    | Outlined, Squiggle -> " { } "
    | Outlined, Diamond -> " < > "
  in
  for y = 0 to get_num c - 1 do
    print_string (color_code ^ shad_shape c ^ reset_code);
    ignore y
  done

let y_to_letter = function
  | 0 -> "A"
  | 1 -> "B"
  | 2 -> "C"
  | 3 -> "D"
  | _ -> ""

let print_grid g =
  print_newline ();
  for x = 0 to get_rows g - 1 do
    for y = 0 to get_cols g - 1 do
      print_string ("        " ^ y_to_letter x ^ string_of_int y ^ "  |");
      let card = get_card g x y in
      match card with
      | None -> print_string "               |"
      | Some c ->
          card_rep c;
          print_string (num_spacing c);
          print_string "|"
    done;
    print_newline ();
    print_newline ()
  done

type game_state = {
  available_cards : Card.t list;
  used_cards : Card.t list;
  grid : Setgame.t;
}

(** all the timer references *)

let total_start_time = ref 0.0
let set_start_time = ref 0.0
let countdown_time = ref 0.0
let start_total_timer () = total_start_time := Unix.gettimeofday ()
let start_set_timer () = set_start_time := Unix.gettimeofday ()

let start_countdown_timer minutes =
  countdown_time := Unix.gettimeofday () +. (float_of_int minutes *. 60.)

(** [initial_state ()] creates initial state for mode 1. *)
let initial_state () =
  start_total_timer ();
  { available_cards = all_cards; used_cards = []; grid = create 4 3 }

(** [restore_available_cards state] is the [state] updated with available cards
    and cards in the grid. *)
let restore_available_cards state =
  { state with available_cards = state.available_cards @ to_list state.grid }

(** [initialize_game2 state] updates [state] for redrawn cards. *)
let initialize_game2 state =
  let state = restore_available_cards state in
  let twelve_cards = pick_cards 12 state.available_cards [] in
  place_cards state.grid twelve_cards;
  print_grid state.grid;
  {
    state with
    available_cards = remove_from_all_cards twelve_cards state.available_cards;
  }

(** [get_user_input state] is the string of input for mode 1. *)
let get_user_input state =
  print_endline
    ("Please enter the positions of three cards (e.g., A0, A1, A2).\n"
   ^ "Available cards: "
    ^ string_of_int (List.length state.available_cards)
    ^ ", Used cards: "
    ^ string_of_int (List.length state.used_cards));
  read_line ()

(** [check_set_exists state] prints whether a set exists and updates the state. *)
let check_set_exists state =
  if set_exists state.grid then begin
    print_endline "Incorrect. A set does exist. Try again.";
    state
  end
  else begin
    print_endline "No set exists; cards have been redrawn.";
    initialize_game2 state
  end

let duration start =
  let current_time = Unix.gettimeofday () in
  current_time -. !start

(** [write_time_to_file username elapsed_time] adds [username] and
    [elapsed_time] to the file. *)
let write_time_to_file username elapsed_time =
  let channel =
    open_out_gen
      [ Open_append; Open_creat ]
      0o666 "leaderboard_elapsed_time.txt"
  in
  Printf.fprintf channel "Username: %s | " username;
  Printf.fprintf channel "Total elapsed time: %02d:%02d\n"
    (int_of_float (elapsed_time /. 60.))
    (int_of_float (mod_float elapsed_time 60.));
  close_out channel

(** [write_sets_per_minute username sets_per_minute] adds [username] and
    [sets_per_minute] to the file. *)
let write_sets_per_minute_to_file username sets_per_minute =
  let channel =
    open_out_gen
      [ Open_append; Open_creat ]
      0o666 "leaderboard_sets_per_minute.txt"
  in
  Printf.fprintf channel "Username: %s | " username;
  Printf.fprintf channel "Total sets per minutes: %f\n" sets_per_minute;
  close_out channel

(** [display_file_contents filename] displays contents of [filename]. *)
let display_file_contents filename =
  let channel = open_in filename in
  try
    while true do
      let line = input_line channel in
      print_endline line
    done
  with End_of_file -> close_in channel

(** [show_leaderboard mode] displays the correct leaderboard based on [mode]. *)
let show_leaderboard mode =
  if mode = "timer" then display_file_contents "leaderboard_elapsed_time.txt"
  else display_file_contents "leaderboard_sets_per_minute.txt"

(** Asks user if they want to see leaderboard. *)
let ask_user_for_dashboard mode =
  print_endline
    "Would you like to see the current leaderboard for your specified mode? 1. \
     Yes 2. No";
  let response = read_line () in
  if response = "Yes" || response = "1" || response = "1." then
    show_leaderboard mode
  else print_endline "Noted. Let us proceed with the game"

(** [choose_mode ()] creates the string for input choosing the game mode. *)
let choose_mode () =
  print_endline "Choose a game mode:";
  print_endline
    "1. Finish the entire game (81 cards) and record the time taken.";
  print_endline
    "2. Record how many sets you can find within a certain time limit and \
     record the sets per minute.";
  print_endline "3. Play the game without a timer, pressure free! :)";
  print_string "Enter 1 or 2 or 3: ";
  read_line ()

(** This version of the game shows the time after a set. *)
let rec main_game_loop2 state username =
  start_set_timer ();
  if List.length state.available_cards = 0 then (
    let total_elapsed = duration total_start_time in
    write_time_to_file username total_elapsed;
    print_endline "Congrats, you have won!")
  else begin
    match get_user_input state with
    | "No set" -> main_game_loop2 (check_set_exists state) username
    | "Cheat" ->
        Printf.printf "\nThe cards in the set are: \n%s\n\n" (cheat state.grid);
        main_game_loop2 state username
    | "Hint" ->
        Printf.printf "\n%s\n\n" (hint state.grid);
        main_game_loop2 state username
    | input -> (
        try
          let positions = parse_input input in
          let cards =
            List.filter_map
              (fun position ->
                get_card state.grid (fst position) (snd position))
              positions
          in
          if
            List.length cards = 3
            && is_set (List.nth cards 0) (List.nth cards 1) (List.nth cards 2)
          then begin
            let set_elapsed = duration set_start_time in
            let total_elapsed = duration total_start_time in
            Printf.printf
              "Valid set! Removing cards... \n\
              \ Time for this set: %02d:%02d. Total time elapsed in game: \
               %02d:%02d\n"
              (int_of_float (set_elapsed /. 60.))
              (int_of_float (mod_float set_elapsed 60.))
              (int_of_float (total_elapsed /. 60.))
              (int_of_float (mod_float total_elapsed 60.));
            remove_cards state.grid positions;
            if List.length state.available_cards <> 0 then begin
              let new_cards = pick_cards 3 state.available_cards [] in
              refill_cards state.grid new_cards positions;
              print_grid state.grid;

              main_game_loop2
                {
                  state with
                  available_cards =
                    remove_from_all_cards new_cards state.available_cards;
                  used_cards = state.used_cards @ cards;
                }
                username
            end
          end
          else begin
            print_endline "Invalid set, try again.";
            main_game_loop2 state username
          end
        with Invalid_argument msg ->
          Printf.printf "\n%s\n" msg;
          main_game_loop2 state username)
  end

let grid = create 4 3
let unseen_cards = ref all_cards
let available_cards = ref all_cards
let used_cards = ref []

(** Initialize the game with twelve random cards and print the initial grid. *)
let initialize_game unseen_cards available_cards =
  let twelve_cards = pick_cards 12 available_cards [] in
  place_cards grid twelve_cards;
  print_grid grid;
  unseen_cards := remove_from_all_cards twelve_cards available_cards

(** This version of the game does not use a timer. *)
let rec main_game_loop unseen_cards available_cards used_cards =
  if List.length !available_cards == 0 then
    print_endline "Congratulations, you have won!"
  else (
    print_endline
      ("Please enter the positions of three cards (e.g. A0, A1, A2).\n\
        Unseen cards: "
      ^ string_of_int (List.length !unseen_cards)
      ^ " Available cards: "
      ^ string_of_int (List.length !available_cards)
      ^ " Used cards: "
      ^ string_of_int (List.length !used_cards));
    let input = read_line () in
    match input with
    | "No set" ->
        if set_exists grid then (
          print_endline "Incorrect. A set does exist.";
          main_game_loop unseen_cards available_cards used_cards)
        else (
          print_endline "No set exists; cards have been redrawn.";
          initialize_game unseen_cards !available_cards;
          main_game_loop unseen_cards available_cards used_cards)
    | "Cheat" ->
        Printf.printf "\nThe cards in the set are: \n%s\n\n" (cheat grid);
        main_game_loop unseen_cards available_cards used_cards
    | "Hint" ->
        Printf.printf "\n%s\n\n" (hint grid);
        main_game_loop unseen_cards available_cards used_cards
    | _ -> (
        try
          let positions = parse_input input in
          let cards =
            List.filter_map
              (fun position -> get_card grid (fst position) (snd position))
              positions
          in
          if
            List.length cards = 3
            && is_set (List.nth cards 0) (List.nth cards 1) (List.nth cards 2)
          then begin
            print_endline "Valid set! Removing cards...";
            remove_cards grid positions;
            used_cards := List.append !used_cards cards;
            available_cards := remove_from_all_cards cards !available_cards;
            if List.length !unseen_cards <> 0 then (
              let new_cards =
                pick_cards (List.length positions) !unseen_cards []
              in
              refill_cards grid new_cards positions;
              unseen_cards := remove_from_all_cards new_cards !unseen_cards);
            print_grid grid;
            main_game_loop unseen_cards available_cards used_cards
          end
          else begin
            print_endline "Invalid set, try again.";
            main_game_loop unseen_cards available_cards used_cards
          end
        with Invalid_argument msg ->
          Printf.printf "\n%s\n" msg;
          main_game_loop unseen_cards available_cards used_cards))

(** This version of the game allows the user to set a time limit. *)
let main_game_loop_timer state sets_found time_limit username =
  start_countdown_timer time_limit;
  (*start_set_timer ();*)
  let rec loop state sets_found =
    let remaining_time = !countdown_time -. Unix.gettimeofday () in
    if remaining_time <= 0.0 then (
      Printf.printf "Time's up! You found %d sets. \n" sets_found;
      write_sets_per_minute_to_file username
        (float_of_int sets_found /. float_of_int time_limit))
    else begin
      start_set_timer ();
      match get_user_input state with
      | "No set" -> loop (check_set_exists state) sets_found
      | "Cheat" ->
          Printf.printf "\nThe cards in the set are: \n%s\n\n"
            (cheat state.grid);
          loop state sets_found
      | "Hint" ->
          Printf.printf "\n%s\n\n" (hint state.grid);
          loop state sets_found
      | input -> (
          try
            let positions = parse_input input in
            let cards =
              List.filter_map
                (fun position ->
                  get_card state.grid (fst position) (snd position))
                positions
            in
            if
              List.length cards = 3
              && is_set (List.nth cards 0) (List.nth cards 1) (List.nth cards 2)
            then begin
              let remaining_time = !countdown_time -. Unix.gettimeofday () in
              Printf.printf
                "Valid set! Removing cards... \nTime left: %02d:%02d"
                (int_of_float (remaining_time /. 60.))
                (int_of_float (mod_float remaining_time 60.));

              remove_cards state.grid positions;
              if List.length state.available_cards <> 0 then begin
                let new_cards = pick_cards 3 state.available_cards [] in
                refill_cards state.grid new_cards positions;
                print_grid state.grid;
                loop
                  {
                    state with
                    available_cards =
                      remove_from_all_cards new_cards state.available_cards;
                    used_cards = state.used_cards @ cards;
                  }
                  (sets_found + 1)
              end
            end
            else begin
              print_endline "Invalid set, try again.";
              loop state sets_found
            end
          with Invalid_argument msg ->
            Printf.printf "\n%s\n" msg;
            loop state sets_found)
    end
  in
  loop state sets_found

(** Help instructions for user, shown with -h argument. *)
let help () =
  print_endline
    "Welcome to Set! \n\
    \ These are the instructions of the game: This is a one-player game. The \
     objective of the game is to identify a SET of 3 cards from the 12 cards \
     shown. There are 81 cards in total. Each cards has four features, which \
     can vary. \n\
    \ The shape can be either Oval, Squiggle, or Diamond. \n\
    \ The color can be either Red, Purple, or Green. \n\
    \ The number can be either 1, 2, or 3. \n\
    \ The shading can be either Solid, Striped, or Outlined. \n\
    \ A SET consists of three cards in which all of the individual features \
     across those three cards are the same or all are different. Once you have \
     identified the three cards, they will be removed and three new cards will \
     be replenished from the deck. \n\
    \ To play the game, you need to enter a list of coordinates with spaces in \
     between. For example: A0, A1, A2.\n\
    \ If you believe that there is no set among the cards displayed, you can \
     type 'No set' into the terminal. If you are correct and there is no set, \
     the cards will be redrawn. If you are incorrect and there indeed exists a \
     set, you will be prompted to try again. \n\
    \ You will be prompted to choose from 3 different modes: time shown, time \
     limited, no time.\n\
    \ While playing, if you would like to receive a random hint for what the \
     set is, enter 'Hint' instead of card coordinates. If you give up and \
     would like to be told the attributes of the set, enter 'Cheat'. "

(** Main game loop handling user interactions. *)
let rec main_interactive () =
  let mode = choose_mode () in
  print_endline "What is your username?:";
  let username = read_line () in
  match mode with
  | "1" ->
      ask_user_for_dashboard "timer";
      print_endline
        "\n\
         Welcome to Set! Note that the game will time how long it takes for \
         you to complete all 81 cards (27 sets). Please select three cards \
         that form a set!";
      main_game_loop2 (initialize_game2 (initial_state ())) username
  | "2" ->
      ask_user_for_dashboard "sets per minute";
      print_endline
        "The game will measure how many sets you make per minute. Enter the \
         time limit in minutes.";
      let minutes = read_int () in
      print_endline
        "\nWelcome to Set! Please select three cards that form a set!";
      main_game_loop_timer
        (initialize_game2 (initial_state ()))
        0 minutes username
  | "3" ->
      print_endline
        "\n\
         Welcome to Set! Please select three cards that form a set! Remember, \
         you are not being timed, so try to relax. ";
      initialize_game unseen_cards !available_cards;
      main_game_loop unseen_cards available_cards used_cards
  | _ ->
      print_endline "\nInvalid mode selected. Please enter 1, 2, or 3.\n";
      main_interactive ()

(** Handles user arguments. *)
let () =
  match Array.length Sys.argv with
  | 1 -> main_interactive ()
  | 2 -> if Sys.argv.(1) = "-h" then help ()
  | _ ->
      print_endline
        "You need either one argument for help or no arguments to play the \
         game."
