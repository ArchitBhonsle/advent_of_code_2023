open! Core

let data = In_channel.(input_all stdin) |> String.strip

type operation =
  | Rem
  | Add of int

type step =
  { label : string
  ; op : operation
  }

let steps =
  data
  |> String.split ~on:','
  |> List.map ~f:(fun s ->
    let split = String.split_on_chars s ~on:[ '-'; '=' ] in
    if String.contains s '='
    then { label = List.hd_exn split; op = Add (split |> List.last_exn |> int_of_string) }
    else { label = List.hd_exn split; op = Rem })
;;

let hash_label label =
  String.fold label ~init:0 ~f:(fun acc c -> Int.rem ((acc + Char.to_int c) * 17) 256)
;;

let boxes =
  steps
  |> List.fold
       ~init:(List.init 256 ~f:(fun _ -> []))
       ~f:(fun acc step ->
         List.mapi acc ~f:(fun idx box ->
           if idx = hash_label step.label
           then (
             match step.op with
             | Rem ->
               List.filter box ~f:(fun lens ->
                 lens |> fst |> String.equal step.label |> not)
             | Add focal_length ->
               if List.count box ~f:(fun p -> p |> fst |> String.equal step.label) <> 0
               then
                 List.map box ~f:(fun p ->
                   if p |> fst |> String.equal step.label then fst p, focal_length else p)
               else (step.label, focal_length) :: box)
           else box))
;;

let () =
  boxes
  |> List.map ~f:List.rev
  |> List.map ~f:(fun box ->
    box
    |> List.mapi ~f:(fun idx curr -> (idx + 1) * snd curr)
    |> List.fold ~init:0 ~f:( + ))
  |> List.mapi ~f:(fun idx power -> (idx + 1) * power)
  |> List.fold ~init:0 ~f:( + )
  |> printf "%d\n"
;;
