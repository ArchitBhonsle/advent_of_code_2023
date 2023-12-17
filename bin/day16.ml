open! Core

let lines = In_channel.(input_lines stdin)
let nrows = lines |> List.length
let ncols = lines |> List.hd_exn |> String.length

type cell =
  | Empty
  | VerticalSplitter
  | HorizontalSplitter
  | LeadingReflector
  | LaggingReflector

let layout = Array.make_matrix ~dimx:nrows ~dimy:ncols Empty

let () =
  lines
  |> List.iteri ~f:(fun i row ->
    row
    |> String.iteri ~f:(fun j c ->
      layout.(i).(j)
        <- (match c with
            | '.' -> Empty
            | '|' -> VerticalSplitter
            | '-' -> HorizontalSplitter
            | '\\' -> LeadingReflector
            | '/' -> LaggingReflector
            | _ -> "unreachable" |> failwith |> raise)))
;;

type direction =
  | Right
  | Left
  | Up
  | Down
[@@deriving compare, sexp, hash]

type sim_call =
  { i : int
  ; j : int
  ; dir : direction
  }
[@@deriving compare, sexp, hash]

module Vis = struct
  type t = sim_call

  let compare = compare_sim_call
  let sexp_of_t = sexp_of_sim_call
  let hash = hash_sim_call
end

let visited = Hashtbl.create (module Vis)

let rec sim i j dir =
  if i < 0 || j < 0 || i >= nrows || j >= ncols || Hashtbl.mem visited { i; j; dir }
  then ()
  else (
    Hashtbl.add_exn visited ~key:{ i; j; dir } ~data:();
    let rec helper i j dir cel =
      match cel with
      | Empty ->
        (match dir with
         | Right -> sim i (j + 1) dir
         | Left -> sim i (j - 1) dir
         | Up -> sim (i - 1) j dir
         | Down -> sim (i + 1) j dir)
      | VerticalSplitter ->
        (match dir with
         | Right | Left ->
           helper i j Up Empty;
           helper i j Down Empty
         | _ -> helper i j dir Empty)
      | HorizontalSplitter ->
        (match dir with
         | Up | Down ->
           helper i j Right Empty;
           helper i j Left Empty
         | _ -> helper i j dir Empty)
      | LeadingReflector ->
        (match dir with
         | Right -> helper i j Down Empty
         | Left -> helper i j Up Empty
         | Up -> helper i j Left Empty
         | Down -> helper i j Right Empty)
      | LaggingReflector ->
        (match dir with
         | Right -> helper i j Up Empty
         | Left -> helper i j Down Empty
         | Up -> helper i j Right Empty
         | Down -> helper i j Left Empty)
    in
    helper i j dir layout.(i).(j))
;;

let run_sim i j dir =
  Hashtbl.clear visited;
  sim i j dir;
  let touched = Array.make_matrix ~dimx:nrows ~dimy:ncols 0 in
  Hashtbl.iter_keys visited ~f:(fun k -> touched.(k.i).(k.j) <- 1);
  touched |> Array.map ~f:(Array.fold ~init:0 ~f:( + )) |> Array.fold ~init:0 ~f:( + )
;;

let () =
  let max = ref 0 in
  for i = 0 to nrows do
    max := Int.max !max (run_sim i 0 Right);
    max := Int.max !max (run_sim i (ncols - 1) Left)
  done;
  for j = 0 to ncols do
    max := Int.max !max (run_sim 0 j Down);
    max := Int.max !max (run_sim (nrows - 1) j Up)
  done;
  printf "%d\n" !max
;;
