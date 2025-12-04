let parse_input input =
  let input_trimmed = String.trim input in
  let raw_positions = String.split_on_char ',' input_trimmed in
  let positions = List.map String.trim raw_positions in
  if List.length positions <> 3 then
    raise
      (Invalid_argument
         "Exactly three positions must be provided (e.g., 'A0, B1, C2').\n");
  let unique_positions =
    List.fold_right
      (fun x acc -> if List.mem x acc then acc else x :: acc)
      positions []
  in
  if List.length unique_positions <> 3 then
    raise
      (Invalid_argument
         "Duplicate positions are not allowed. Each position must be unique.\n");
  List.map
    (fun pos ->
      if String.length pos <> 2 then
        raise
          (Invalid_argument
             "Invalid format: Positions must be two characters long (e.g., \
              'A1').\n");
      let x_char = pos.[0] in
      let y_char = pos.[1] in
      if not (x_char >= 'A' && x_char <= 'D') then
        raise (Invalid_argument "Invalid row: Must be between 'A' and 'D'.\n");
      if not (y_char >= '0' && y_char <= '2') then
        raise
          (Invalid_argument "Invalid column: Must be between '0' and '2'.\n");
      let x = Char.code x_char - Char.code 'A' in
      let y =
        try int_of_string (String.make 1 y_char)
        with Failure _ ->
          raise (Invalid_argument "Invalid number format for column.\n")
      in
      (x, y))
    unique_positions
