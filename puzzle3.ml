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
;;

let digit c = Char.code c - Char.code '0'

let largest_jolt s =
  let n = String.length s in
  let memo = Hashtbl.create 10000 in
  let rec f i cnt =
    if cnt = 0
    then 0
    else if i = n
    then min_int
    else (
      match Hashtbl.find_opt memo (i, cnt) with
      | Some v -> v
      | None ->
        let take =
          (digit s.[i] * int_of_float (10. ** float_of_int (cnt - 1)))
          + f (i + 1) (cnt - 1)
        in
        let skip = f (i + 1) cnt in
        let res = max take skip in
        Hashtbl.add memo (i, cnt) res;
        res)
  in
  f 0 12
;;

let () =
  "puzzle3.in"
  |> lines
  |> Seq.fold_left (fun acc s -> acc + largest_jolt s) 0
  |> print_int
;;
