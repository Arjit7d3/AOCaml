let lines filename : string Seq.t =
  let ic = open_in filename in
  let rec next () =
    match In_channel.input_line ic with
    | Some line -> Seq.Cons (line, next)
    | None ->
        close_in ic;
        Seq.Nil
  in
  next

let count_zeros filename =
  lines filename
  |> Seq.filter (fun s -> s <> "")
  |> Seq.map (fun s ->
      let x = int_of_string (String.sub s 1 (String.length s - 1)) in
      if s.[0] = 'R' then x else -x)
  |> Seq.fold_left
       (fun (zeros, pos) x ->
         let p = pos in
         let k = abs x in

         let d = if x > 0 then (100 - p) mod 100 else p mod 100 in
         let first = if d = 0 then 100 else d in

         let hits = if k < first then 0 else 1 + ((k - first) / 100) in

         let new_pos =
           if x >= 0 then (p + k) mod 100 else (p - (k mod 100) + 100) mod 100
         in

         (zeros + hits, new_pos))
       (0, 50)
  |> fst

let () =
  print_int (count_zeros "puzzle1_a.in");
  print_newline ()
