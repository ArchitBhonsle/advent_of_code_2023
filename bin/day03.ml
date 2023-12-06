open! Core

let lines = In_channel.(input_lines stdin)
let re = Re.compile (Re.Pcre.re {|(\d+)|})

type record =
  { number : int
  ; start : int
  ; stop : int
  ; line : int
  }

let records =
  lines
  |> List.map ~f:(Re.all re)
  |> List.mapi ~f:(fun i gs ->
    List.map gs ~f:(fun g ->
      { number = int_of_string (Re.Group.get g 0)
      ; start = Re.Group.start g 0
      ; stop = Re.Group.stop g 0
      ; line = i
      }))
  |> List.concat
;;

let matrix = Array.of_list lines

let is_gear p =
  let i = fst p in
  let j = snd p in
  if i < 0 || i >= Array.length matrix || j < 0 || j >= String.length (Array.get matrix 0)
  then false
  else Char.equal (String.get (Array.get matrix i) j) '*'
;;

let get_gears_locations r =
  let outer = List.init (r.stop - r.start + 2) ~f:(fun i -> r.start - 1 + i) in
  let inner = List.init 3 ~f:(fun i -> r.line - 1 + i) in
  List.map outer ~f:(fun j -> List.map inner ~f:(fun i -> i, j))
  |> List.concat
  |> List.filter ~f:is_gear
;;

module IntPair = struct
  module T = struct
    type t = int * int

    let compare x y = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare x y
    let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t
    let t_of_sexp = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp
    let hash = Hashtbl.hash
  end

  include T
  include Comparable.Make (T)
end

let potential_gears = Hashtbl.create (module IntPair)

let () =
  records
  |> List.map ~f:(fun r -> r.number, get_gears_locations r)
  |> List.iter ~f:(fun p ->
    let number = fst p in
    let gears = snd p in
    List.iter gears ~f:(fun c ->
      Hashtbl.update potential_gears c ~f:(fun v ->
        match v with
        | Some l -> number :: l
        | None -> [ number ])))
;;

let () =
  potential_gears
  |> Hashtbl.to_alist
  |> List.map ~f:(fun e -> snd e)
  |> List.filter ~f:(fun l -> List.length l = 2)
  |> List.map ~f:(List.fold ~init:1 ~f:( * ))
  |> List.fold ~init:0 ~f:( + )
  |> printf "%d\n"
;;
