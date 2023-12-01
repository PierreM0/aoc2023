(* from the internet *)
let string_of_char c = String.make 1 c

(* Converts a string to a list of chars *)
let explode str =
  let rec explode_inner cur_index chars =
    if cur_index < String.length str then
      let new_char = str.[cur_index] in
      explode_inner (cur_index + 1) (chars @ [ new_char ])
    else chars
  in
  explode_inner 0 []

let is_num c = '0' <= c && c <= '9'

let from_written_num s =
  match explode s with
  | 'o' :: 'n' :: 'e' :: _ -> ("1", 1)
  | 't' :: 'w' :: 'o' :: _ -> ("2", 1)
  | 's' :: 'i' :: 'x' :: _ -> ("6", 1)
  | 'f' :: 'o' :: 'u' :: 'r' :: _ -> ("4", 1)
  | 'f' :: 'i' :: 'v' :: 'e' :: _ -> ("5", 1)
  | 'n' :: 'i' :: 'n' :: 'e' :: _ -> ("9", 1)
  | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ -> ("3", 1)
  | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ -> ("7", 1)
  | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ -> ("8", 1)
  | [] | _ -> ("", 1)

let get_num s =
  let l = String.length s in
  let rec loop (i : int) : string =
    if l != 0 then
      if i != l then
        if i > l - 1 then if is_num s.[i] then string_of_char s.[i] else ""
        else if is_num s.[i] then string_of_char s.[i] ^ loop (i + 1)
        else
          let num, index =
            from_written_num (String.sub s i (String.length s - i))
          in
          num ^ loop (index + i)
      else ""
    else "0"
  in
  let x = loop 0 in
  string_of_char x.[0] ^ string_of_char x.[String.length x - 1]

let get_sum s =
  let l = String.split_on_char '\n' s in
  let rec sum l =
    match l with
    | [] -> 0
    | x :: t ->
        let a = get_num x in
        print_endline (x ^ "=>" ^ a);
        int_of_string a + sum t
  in
  sum l

let read_whole_file filename =
  let channel = open_in_bin filename in
  let s = really_input_string channel (in_channel_length channel) in
  close_in channel;
  s

let () =
  if Array.length Sys.argv != 2 then
    Printf.eprintf "Error: not enought or too many arguments: file path needed"
  else print_int (get_sum (read_whole_file Sys.argv.(1)))
