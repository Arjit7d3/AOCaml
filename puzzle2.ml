let read_text filename = filename |> open_in |> In_channel.input_all

let check_invalid s =
  let len = String.length s in
  match len mod 2 with
  | 0 ->
      let first_half = String.sub s 0 (len / 2) in
      let second_half = String.sub s (len / 2) (len / 2) in
      first_half = second_half
  | _ -> false

let () =
  "puzzle2.in" |> read_text |> String.trim |> String.split_on_char ','
  |> List.map (fun s ->
      match String.split_on_char '-' s with
      | [ a; b ] -> (a, b)
      | _ -> failwith "invalid input")
  |> List.fold_left
       (fun acc (l, r) ->
         let fold_range f acc x y =
           let rec loop acc i = if i > y then acc else loop (f acc i) (i + 1) in
           loop acc x
         in
         fold_range
           (fun acc i ->
             if check_invalid (string_of_int i) then acc + i else acc)
           acc (int_of_string l) (int_of_string r))
       0
  |> print_int
