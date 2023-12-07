open! Core

let lines = In_channel.(input_lines stdin)
let explode s = List.init (String.length s) ~f:(fun i -> String.get s i)

let pairs =
  lines
  |> List.map ~f:(fun line ->
    let split = line |> String.strip |> String.split ~on:' ' in
    explode (List.nth_exn split 0), int_of_string (List.nth_exn split 1))
;;

let hand_type hand =
  hand
  |> List.sort_and_group ~compare:Char.compare
  |> List.map ~f:List.length
  |> List.sort ~compare:Int.compare
  |> List.rev
  |> function
  | [ 5 ] -> 7
  | [ 4; 1 ] -> 6
  | [ 3; 2 ] -> 5
  | [ 3; 1; 1 ] -> 4
  | [ 2; 2; 1 ] -> 3
  | [ 2; 1; 1; 1 ] -> 2
  | [ 1; 1; 1; 1; 1 ] -> 1
  | _ -> raise (Invalid_argument "Should not be possible")
;;

let cards = [ '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'Q'; 'K'; 'A' ]
let card_value card = fst (List.findi_exn cards ~f:(fun _ c -> Char.equal card c))

let compare_hands a b =
  match Int.compare (hand_type a) (hand_type b) with
  | 0 ->
    List.compare Int.compare (a |> List.map ~f:card_value) (b |> List.map ~f:card_value)
  | _ as x -> x
;;

let compare_pair a b = compare_hands (fst a) (fst b)

let () =
  pairs
  |> List.sort ~compare:compare_pair
  |> List.mapi ~f:(fun idx p -> (idx + 1) * snd p)
  |> List.fold ~init:0 ~f:( + )
  |> printf "%d\n"
;;
