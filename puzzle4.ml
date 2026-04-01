let lines filename =
  let ic = open_in filename in
  let rec next () =
    match In_channel.input_line ic with
    | Some line -> Seq.Cons (line, next)
    | None ->
      close_in ic;
      Seq.Nil
  in
  next
;;

let parse_grid filename =
  filename
  |> lines
  |> Seq.map (fun line ->
    line
    |> String.to_seq
    |> Array.of_seq
    |> Array.map (function
      | '@' -> 1
      | '.' -> 0
      | _ -> failwith "invalid char"))
  |> Array.of_seq
;;

type 'a grid =
  { data : 'a array array
  ; rows : int
  ; cols : int
  }

let make_grid data =
  let rows = Array.length data in
  let cols = if rows = 0 then 0 else Array.length data.(0) in
  { data; rows; cols }
;;

let extract g (i, j) =
  if i < 0 || j < 0 || i >= g.rows || j >= g.cols then 0 else g.data.(i).(j)
;;

let directions = [ -1, -1; -1, 0; -1, 1; 0, -1; 0, 1; 1, -1; 1, 0; 1, 1 ]

let neighbors_sum g (i, j) =
  directions
  |> List.map (fun (di, dj) -> extract g (i + di, j + dj))
  |> List.fold_left ( + ) 0
;;

let extend f g =
  { data = Array.init g.rows (fun i -> Array.init g.cols (fun j -> f g (i, j)))
  ; rows = g.rows
  ; cols = g.cols
  }
;;

let rule g (i, j) =
  let v = extract g (i, j) in
  Bool.to_int (v = 1 && neighbors_sum g (i, j) < 4)
;;

let sum_grid g =
  Array.fold_left (fun acc row -> acc + Array.fold_left ( + ) 0 row) 0 g.data
;;

let solve filename =
  let g = filename |> parse_grid |> make_grid in
  g |> extend rule |> sum_grid
;;

let () =
  "puzzle4.in" |> solve |> print_int;
  print_newline ()
;;
