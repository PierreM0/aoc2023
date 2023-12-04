(* Converts a string to a list of chars *)
let explode str =
  let rec explode_inner cur_index chars =
    if cur_index < String.length str then
      let new_char = str.[cur_index] in
      explode_inner (cur_index + 1) (chars @ [ new_char ])
    else chars
  in
  explode_inner 0 []

let _char_is_next_to_symbol _char_list_list _i _j = true
let print_bool b = if b then print_endline "true" else print_endline "false"
let not_a_number char = char < '0' || '9' < char
let is_symbol char = char != '.' && not_a_number char

let access_element list_list x y =
  try
    let line = List.nth list_list x in
    let item = List.nth line y in
    Some item
  with Failure _ | Invalid_argument _ -> None

let rec _parcourir_lignes char_list_list line_nbr =
  let rec go_through_line line length line_nbr =
    match line with
    | [] -> ()
    | _ :: xs ->
        (* traitement de l'élément x *)
        let posi = length - List.length xs - 1 in
        (* Printf.printf "pos de '%c': %d %d" x line_nbr posi;*)
        print_bool (traiter_voisins (line_nbr, posi));
        go_through_line xs length line_nbr
  and traiter_voisins (x, y) =
    let res =
      match access_element char_list_list x y with
      | Some item ->
          Printf.printf " x:%d y:%d -> " x y;
          print_char item;
          is_symbol item
      | None -> true
    in
    res
  in

  match char_list_list with
  | [] -> ()
  | ligne :: autre_ligne ->
      let line_length = List.length ligne in
      go_through_line ligne line_length line_nbr;
      print_char '\n';
      _parcourir_lignes autre_ligne (line_nbr + 1)

let part_1 input =
  let lines = String.split_on_char '\n' input in
  let lines = List.map explode lines in
  Printf.printf "there are %d lines. Chaque ligne a la taille %d\n"
    (List.length lines)
    (List.length (List.nth lines 0));
  let rec getx (x, y) =
    if x < 10 then
      if y < 10 then
        let is_symbol =
          match access_element lines x y with
          | Some i -> is_symbol i
          | None -> false
        in
        getx (x, y + 1)
      else (
        print_char '\n';
        getx (x + 1, 0))
  in
  getx (0, 0);
  _parcourir_lignes lines 0

let read_whole_file filename =
  let channel = open_in_bin filename in
  let s = really_input_string channel (in_channel_length channel) in
  close_in channel;
  s

let rec _sum s = match s with [] -> 0 | x :: t -> x + _sum t

let () =
  if Array.length Sys.argv != 2 then
    Printf.eprintf "Error: not enought or too many arguments: file path needed"
  else part_1 (read_whole_file Sys.argv.(1))
