open! Core;;

let input = In_channel.(input_all stdin);;

let maps_re = {|\w+\-to\-\w+\s+map\:\n((?:\d+\s*)+\n?)|}
    |> Re.Pcre.re
    |> Re.compile
;;
type map = { src: int; dst: int; len: int };;
let parse_map_str map_str = map_str
    |> String.split_lines
    |> List.map ~f:String.strip
    |> List.map ~f:(String.split ~on:' ')
    |> List.map ~f:(List.map ~f:String.strip)
    |> List.map ~f:(List.map ~f:int_of_string)
    |> List.map ~f:(fun l -> { dst = (List.nth_exn l 0); src = (List.nth_exn l 1); len = (List.nth_exn l 2) })
let maps = input
    |> Re.all maps_re
    |> List.map ~f:(fun m -> Re.Group.get m 1)
    |> List.map ~f:String.strip
    |> List.map ~f:parse_map_str
    |> List.map ~f:(List.sort ~compare:(fun a b -> compare a.src b.src))
;;
let maps_to from maplist = 
    maplist
    |> List.rev
    |> List.find ~f:(fun m -> m.src <= from)
    |> (fun m -> match m with
        | Some mi -> mi
        | None -> { src = -1; dst = -1; len = 0 }
        )
    |> (fun { src; dst; len } -> (if src + len > from then from - src + dst else from))
;;


let seeds_re = {|seeds:\s+((?:\d+\s+)*)|}
    |> Re.Pcre.re
    |> Re.compile
;;
let seeds = input
    |> Re.exec seeds_re
    |> (fun m -> Re.Group.get m 1)
    |> String.strip
    |> String.split ~on:' '
    |> List.map ~f:int_of_string
;;

let rec search map_list_list from =
    match map_list_list with
    | [] -> from
    | map_list :: rest -> search rest (maps_to from map_list)
;;
let minimum = seeds
    |> List.map ~f:(search maps)
    |> List.fold ~init:Int.max_value ~f:Int.min
;;

let () = printf "%d\n" minimum;;


