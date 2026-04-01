let has_period s =
  let n = String.length s in
  Seq.init (n - 1) (( + ) 1)
  |> Seq.filter (fun d -> n mod d = 0)
  |> Seq.exists (fun d ->
      let first = String.sub s 0 d in
      let rec check i = i = n || (String.sub s i d = first && check (i + d)) in
      check d )

let () =
  In_channel.with_open_bin "puzzle2.in" In_channel.input_all
  |> String.trim |> String.split_on_char ','
  |> List.map (fun s ->
      match String.split_on_char '-' s with
      | [a; b] ->
          (int_of_string a, int_of_string b)
      | _ ->
          failwith "bad" )
  |> List.fold_left
       (fun acc (l, r) ->
         acc
         + ( Seq.init (r - l + 1) (( + ) l)
           |> Seq.filter (fun x -> has_period (string_of_int x))
           |> Seq.fold_left ( + ) 0 ) )
       0
  |> print_int
