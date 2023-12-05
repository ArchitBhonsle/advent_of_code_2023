open! Core;;

let lines = In_channel.(input_lines stdin);;

let line_re = {|Card\s+(\d+):\s+((?:\s*\d+\s*)*)\s+\|\s+((?:\s*\d+\s*)*)|}
    |> Re.Pcre.re
    |> Re.compile
;;

let whitespace_re = Re.compile (Re.rep1 Re.space);;
let split_on_whitespace line =
    line
    |> String.strip
    |> Re.split whitespace_re
;;

let parse_gains line =
    let result = Re.exec line_re line
    in
    let have = (Re.Group.get result 2) |> split_on_whitespace |> List.map ~f:int_of_string;
    in
    let want = (Re.Group.get result 3) |> split_on_whitespace |> List.map ~f:int_of_string;
    in
    have
    |> List.count ~f:(fun h -> List.exists want ~f:(fun w -> w = h))
;;

let gains = lines
    |> List.map ~f:parse_gains
    |> Array.of_list
;;

let rec calc i = 
    if i >= (Array.length gains) then 0 else
    List.init (Array.get gains i) ~f:(fun x -> x + i + 1)
        |> List.map ~f:calc
        |> List.fold ~init:0 ~f:(+)
        |> (+) 1
;;

let () = List.init (Array.length gains) ~f:(fun x -> x)
    |> List.map ~f:calc
    |> List.fold ~init:0 ~f:(+)
    |> printf "%d\n"
;;
