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

type card = { have: int list; want: int list };;
let parse_line line =
    print_endline line;
    let result = Re.exec line_re line
    in
    {
        have = (Re.Group.get result 2) |> split_on_whitespace |> List.map ~f:int_of_string;
        want = (Re.Group.get result 3) |> split_on_whitespace |> List.map ~f:int_of_string;
    }
;;

let card_score card =
    card.have
    |> List.count ~f:(fun h -> List.exists card.want ~f:(fun w -> w = h))
    |> function
        | 0 -> 0
        | i -> i - 1
    |> Int.pow 2

let () = lines
    |> List.map ~f:parse_line
    |> List.map ~f:card_score
    |> List.fold ~f:(+) ~init:0
    |> printf "%d\n";
;;
