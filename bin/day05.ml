open! Core

let input = In_channel.(input_all stdin)

type range =
  { beg : int
  ; len : int
  }

type translation =
  { dst : int
  ; src : range
  }

(* read seeds *)
let seeds_re = {|seeds:\s+((?:\d+\s+)*)|} |> Re.Pcre.re |> Re.compile

let rec into_pairs l =
  match l with
  | [] -> []
  | a :: b :: rest -> (a, b) :: into_pairs rest
  | _ -> raise (Invalid_argument "the list had odd number of elements")
;;

let seeds =
  input
  |> Re.exec seeds_re
  |> (fun m -> Re.Group.get m 1)
  |> String.strip
  |> String.split ~on:' '
  |> List.map ~f:int_of_string
  |> into_pairs
  |> List.map ~f:(fun p -> { beg = fst p; len = snd p })
;;

(* read stages *)
let stages_re = {|\w+\-to\-\w+\s+map\:\n((?:\d+\s*)+\n?)|} |> Re.Pcre.re |> Re.compile

let parse_stages_str stages_str =
  stages_str
  |> String.split_lines
  |> List.map ~f:String.strip
  |> List.map ~f:(String.split ~on:' ')
  |> List.map ~f:(List.map ~f:String.strip)
  |> List.map ~f:(List.map ~f:int_of_string)
  |> List.map ~f:(fun l ->
    { dst = List.nth_exn l 0; src = { beg = List.nth_exn l 1; len = List.nth_exn l 2 } })
;;

let stages =
  input
  |> Re.all stages_re
  |> List.map ~f:(fun m -> Re.Group.get m 1)
  |> List.map ~f:String.strip
  |> List.map ~f:parse_stages_str
;;

(* break `this` range into at most 3 parts where none intersect `using`s endpoints *)
let break_range this using =
  let a_end = using.beg + using.len in
  let b_end = this.beg + this.len in
  [ Int.min using.beg this.beg, Int.min a_end this.beg
  ; Int.max this.beg using.beg, Int.min a_end b_end
  ; Int.max b_end using.beg, Int.max a_end b_end
  ]
  |> List.map ~f:(fun p -> { beg = fst p; len = snd p - fst p })
  |> List.filter ~f:(fun r -> r.len > 0)
;;

(* translate `this` range using `translations`*)
let translate_range translations this =
  translations
  |> List.find ~f:(fun t -> t.src.beg <= this.beg && this.beg <= t.src.beg + t.src.len)
  |> function
  | Some m -> { beg = this.beg - m.src.beg + m.dst; len = this.len }
  | None -> this
;;

(* run the simulation *)
let final =
  stages
  |> List.fold ~init:seeds ~f:(fun curr stage ->
    stage
    |> List.fold ~init:curr ~f:(fun c t ->
      c |> List.map ~f:(break_range t.src) |> List.concat)
    |> List.map ~f:(translate_range stage))
;;

let () =
  final
  |> List.map ~f:(fun r -> r.beg)
  |> List.fold ~init:Int.max_value ~f:Int.min
  |> printf "%d\n"
;;
