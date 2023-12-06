open! Core;;

let input = In_channel.(input_all stdin);;

let compile_pcre re = re |> Re.Pcre.re |> Re.compile;;

let times_re = compile_pcre {|Time:((?:\s+\d+)*)|};;
let distances_re = compile_pcre {|Distance:((?:\s+\d+)*)|};;

let whitespace_re = compile_pcre {|\s+|}
let split_into_ints str = str
    |> String.strip
    |> Re.split whitespace_re
    |> List.map ~f:int_of_string

let times = input |> (Re.exec times_re) |> (fun m -> Re.Group.get m 1) |> split_into_ints;;
let distances = input |> (Re.exec distances_re) |> (fun m -> Re.Group.get m 1) |> split_into_ints;;

let ways = List.map2_exn times distances ~f:(fun t d ->
    List.init (t + 1) ~f:(fun x -> x)
    |> List.count ~f:(fun x -> (x * (t - x)) > d)
)
;;

let () = ways
    |> List.fold ~init:1 ~f:( * )
    |> printf "%d\n"
;;

(* x * (t - x) > d *)
