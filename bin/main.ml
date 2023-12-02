open! Core;;

let lines = In_channel.read_lines (Sys.get_argv ()).(1);;

let words = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"];;
let words_rev = words |> List.map ~f:String.rev;;

let create_re x = Re.compile (Re.alt (Re.digit :: (List.map x ~f:Re.str)));; 
let forward_re = create_re words;;
let backward_re = create_re words_rev;;

let get_num possibilities found = 
    match List.findi possibilities ~f:(fun _ -> fun p -> String.equal p found) with
    | Some idx -> fst idx + 1
    | None -> int_of_string found;;

let parse_line line =
    let forward = get_num words (Re.Group.get (Re.exec forward_re line) 0)
    and backward = get_num words_rev (Re.Group.get (Re.exec backward_re (String.rev line)) 0)
    in
    forward * 10 + backward;;

let () = lines
    |> List.map ~f:parse_line
    |> List.fold ~init:0 ~f:(+)
    |> printf "%d\n"
