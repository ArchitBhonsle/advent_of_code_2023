open! Core

let lines = In_channel.(input_lines stdin)

let split_and_strip ~on x = x |> String.split ~on:on |> List.map ~f:String.strip

let list_to_pair l = l |> List.hd_exn, l |> List.rev |> List.hd_exn;;

(* let parse_game_id part = *)
(*     part *)
(*     |> split_and_strip ~on:' ' *)
(*     |> List.rev *)
(*     |> List.hd_exn *)
(*     |> int_of_string;; *)

type reveal = { r: int; g: int; b: int };;
type game = { (* id: int; *) required: reveal };;

let parse_pull pull_str =
    let count_color = pull_str
    |> split_and_strip ~on:' '
    |> list_to_pair
    in
    let count = count_color |> fst |> int_of_string 
    in
    let default = { r = 0; g = 0; b = 0 }
    in
    match (snd count_color) with
    | "red" -> { default with r=count }
    | "green" -> { default with g=count }
    | "blue" -> { default with b=count }
    | _ -> raise (Invalid_argument "unreachable")
;;

let process_reveals so_far curr = {
    r = Int.max so_far.r curr.r;
    g = Int.max so_far.g curr.g;
    b = Int.max so_far.b curr.b;
};;

let parse_reveal reveal_str =
    reveal_str
    |> split_and_strip ~on:','
    |> List.map ~f:parse_pull
    |> List.fold ~init:{ r = 0; g = 0; b = 0 } ~f:process_reveals;;

let parse_reveals reveals_str =
    reveals_str
    |> split_and_strip ~on:';'
    |> List.map ~f:parse_reveal;;

let parse_game game_str =
    let split = game_str
    |> split_and_strip ~on:':'
    |> list_to_pair 
    in
    (* let game_id_str = fst split  *)
    (* in *)
    let reveals_str = snd split
    in
    { 
        (* id = parse_game_id game_id_str;  *)
        required = (
            reveals_str 
            |> parse_reveals 
            |> List.fold ~init:{ r = 0; g = 0; b = 0 } ~f:process_reveals
        ) 
    }
;;

(* let valid_reveal check = 12 >= check.r && 13 >= check.g && 14 >= check.b;; *)
(* let () = lines *)
(*     |> List.map ~f:parse_game *)
(*     (* |> List.iter ~f:(fun g -> printf "%d %d %d %d\n" g.id g.max.r g.max.g g.max.b);; *) *)
(*     |> List.filter ~f:(fun g -> valid_reveal g.required) *)
(*     (* |> List.iter ~f:(fun g -> printf "%d\n" g.id) *) *)
(*     |> List.map ~f:(fun g -> g.id) *)
(*     |> List.fold ~init:0 ~f:(+) *)
(*     |> printf "%d\n";; *)

let () = lines
    |> List.map ~f:parse_game
    |> List.map ~f:(fun g -> g.required.r * g.required.g * g.required.b)
    |> List.fold ~init:0 ~f:(+)
    |> printf "%d\n";;
