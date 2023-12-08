open! Core

let input = In_channel.(input_all stdin)
let directions_re = {|[RL]+|} |> Re.Pcre.re |> Re.compile
let map_re = {|(?:(\w+) = \((\w+), (\w+)\))|} |> Re.Pcre.re |> Re.compile

let directions =
  input
  |> Re.exec directions_re
  |> (fun g -> Re.Group.get g 0)
  |> fun str -> List.init (String.length str) ~f:(fun i -> String.get str i)
;;

let map =
  input
  |> Re.all map_re
  |> List.map ~f:(fun g -> Re.Group.get g 1, (Re.Group.get g 2, Re.Group.get g 3))
  |> Map.of_alist_exn (module String)
;;

let move start =
  directions
  |> List.fold ~init:start ~f:(fun acc curr ->
    (match curr with
     | 'L' -> fst
     | 'R' -> snd
     | _ -> raise (Invalid_argument "Not possible"))
      (Map.find_exn map acc))
;;

let rec loop start i =
  match start with
  | "ZZZ" -> i
  | _ -> loop (move start) (i + 1)
;;

let () = loop "AAA" 0 |> ( * ) (List.length directions) |> printf "%d\n"
