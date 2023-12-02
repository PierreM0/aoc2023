let rec sum int_list = match int_list with [] -> 0 | x :: t -> x + sum t

let how_many_per_set color set =
  sum
    (List.mapi
       (let f i s =
          match int_of_string_opt s with
          | None -> 0
          | Some n ->
              if String.starts_with ~prefix:color (List.nth set (i + 1)) then n
              else 0
        in
        f)
       set)

let check_each_sets line rmax gmax bmax =
  List.for_all
    (let f set =
       let set = String.split_on_char ' ' set in
       let r = how_many_per_set "red" set in
       let g = how_many_per_set "green" set in
       let b = how_many_per_set "blue" set in
       r <= rmax && g <= gmax && b <= bmax
     in
     f)
    line

let _part1 s =
  let red_max = 12 in
  let green_max = 13 in
  let blue_max = 14 in

  let lines = String.split_on_char '\n' s in
  let lines = List.map (fun line -> String.split_on_char ';' line) lines in
  List.mapi
    (let lines_lenght = List.length lines in
     let f i line =
       if
         check_each_sets line red_max green_max blue_max
         && i != lines_lenght - 1
       then i + 1
       else 0
     in
     f)
    lines

let rec max int_list =
  match int_list with
  | [] -> 0
  | x :: t ->
      let t = max t in
      if x == 0 then t else if x > t then x else t

let get_min_from_each_sets line =
  let sets = List.map (String.split_on_char ' ') line in
  let rgb_per_sets =
    List.map
      (let f set =
         let r = how_many_per_set "red" set in
         let g = how_many_per_set "green" set in
         let b = how_many_per_set "blue" set in
         (r, g, b)
       in
       f)
      sets
  in
  let min_r = max (List.map (fun (a, _, _) -> a) rgb_per_sets) in
  let min_g = max (List.map (fun (_, a, _) -> a) rgb_per_sets) in
  let min_b = max (List.map (fun (_, _, a) -> a) rgb_per_sets) in
  (min_r, min_g, min_b)

let part2 s =
  let lines = String.split_on_char '\n' s in

  let lines = List.map (fun line -> String.split_on_char ';' line) lines in
  let lines_length = List.length lines in
  List.mapi
    (let f i line =
       if i != lines_length - 1 then (
         let r, g, b = get_min_from_each_sets line in
         Printf.printf "r: %d g:%d b:%d\n" r g b;
         r * g * b)
       else 0
     in
     f)
    lines

let read_whole_file filename =
  let channel = open_in_bin filename in
  let s = really_input_string channel (in_channel_length channel) in
  close_in channel;
  s

let () =
  if Array.length Sys.argv != 2 then
    Printf.eprintf "Error: not enought or too many arguments: file path needed"
  else Printf.printf "Answer: %d." (sum (part2 (read_whole_file Sys.argv.(1))))
