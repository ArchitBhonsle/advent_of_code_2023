open! Core

let input = In_channel.(input_all stdin)
let directions_re = {|[RL]+|} |> Re.Pcre.re |> Re.compile
let map_re = {|(?:(\w+) = \((\w+), (\w+)\))|} |> Re.Pcre.re |> Re.compile

let given_directions =
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

let directions = Sequence.cycle_list_exn given_directions

let starts =
  map
  |> Map.keys
  |> List.filter ~f:(fun s -> Char.equal (String.get s (String.length s - 1)) 'A')
;;

let move dir now =
  (match dir with
   | 'L' -> fst
   | 'R' -> snd
   | _ -> raise (Invalid_argument "Not possible"))
    (Map.find_exn map now)
;;

let rec loop dirs i start =
  if Char.equal (String.get start (String.length start - 1)) 'Z'
  then i
  else (
    let head = Sequence.hd_exn dirs in
    let tail = Sequence.tl_eagerly_exn dirs in
    loop tail (i + 1) (move head start))
;;

let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let lcm a b = a * b / gcd a b

let () =
  List.map starts ~f:(loop directions 0) |> List.fold ~init:1 ~f:lcm |> printf "%d\n"
;;
