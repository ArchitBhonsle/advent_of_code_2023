open! Core

let input = In_channel.(input_all stdin)
let compile_pcre re = re |> Re.Pcre.re |> Re.compile
let times_re = compile_pcre {|Time:((?:\s+\d+)*)|}
let distances_re = compile_pcre {|Distance:((?:\s+\d+)*)|}
let whitespace_re = compile_pcre {|\s+|}

let parse str =
  str |> String.strip |> Re.split whitespace_re |> String.concat |> int_of_string
;;

let find_and_parse re input = input |> Re.exec re |> (fun m -> Re.Group.get m 1) |> parse
let time = input |> find_and_parse times_re
let distance = input |> find_and_parse distances_re

let solve time distance =
  let a = 1. in
  let b = -.float_of_int time in
  let c = float_of_int distance in
  let d = (b *. b) -. (4. *. a *. c) in
  if Float.compare d 0. <= 0
  then 0
  else (
    let v = (-.b +. Float.sqrt d) /. (2. *. a) in
    let u = (-.b -. Float.sqrt d) /. (2. *. a) in
    int_of_float (Float.round_down v) - int_of_float (Float.round_up u) + 1)
;;

let () = printf "%d\n" (solve time distance)
