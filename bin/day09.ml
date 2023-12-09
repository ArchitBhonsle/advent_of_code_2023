open! Core

let input =
  In_channel.(input_lines stdin)
  |> List.map ~f:(fun line ->
    line |> String.strip |> String.split ~on:' ' |> List.map ~f:int_of_string)
;;

let rec extrapolate nums =
  if nums |> List.for_all ~f:(Int.equal 0)
  then 0
  else
    List.hd_exn nums
    - extrapolate
        (List.zip_exn (nums |> List.rev |> List.tl_exn |> List.rev) (nums |> List.tl_exn)
         |> List.map ~f:(fun (a, b) -> b - a))
;;

let () = input |> List.map ~f:extrapolate |> List.fold ~init:0 ~f:( + ) |> printf "%d\n"
