open! Core;;

let lines = In_channel.(input_lines stdin);;

let re = Re.compile (Re.Pcre.re {|(\d+)|});;

type record = { number: int; start: int; stop: int; line: int };;
let records = lines
    |> List.map ~f:(Re.all re)
    |> List.mapi ~f:(fun i gs ->
        List.map gs ~f:(fun g -> {
                number = int_of_string (Re.Group.get g 0);
                start = Re.Group.start g 0;
                stop = Re.Group.stop g 0;
                line = i
            })
        )
    |> List.concat
;;

let matrix = Array.of_list lines;;
let check i j =
    if i < 0 || i >= Array.length matrix || j < 0 || j >= String.length (Array.get matrix 0) 
    then false
    else 
        let x = String.get (Array.get matrix i) j
        in 
            if Char.is_digit x || Char.equal x '.' 
            then false
            else true
;;

let check_record r =
    let outer = List.init (r.stop - r.start + 2) ~f:(fun i -> r.start - 1 + i)
    in
    let inner = List.init 3 ~f:(fun i -> r.line - 1 + i)
    in
    List.exists outer ~f:(fun j ->
        List.exists inner ~f:(fun i -> check i j)
    )
;;

let () =
    records
    |> List.filter ~f:check_record
    |> List.map ~f:(fun r -> r.number)
    |> List.fold ~init:0 ~f:(+)
    |> printf "%d\n";;

