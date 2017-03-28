(* For more information: 
   https://ataias.com.br/2017/03/24/compare-triplets-in-ocaml/ *)

open Core.Std
type rating = {
    x1 : int;
    x2 : int;
    x3 : int;
  }

let get_line () = In_channel.input_line In_channel.stdin

(* When parsing with String.split,
  some elements may be empty,
  so we remove them *)
let remove_empty l =
  let not_empty x = x <> "" in
  List.filter ~f:not_empty l

(* Magic happens, returns list of numbers*)                   
let parse line =
  let l = String.split line ~on:' ' in
  let number_str = remove_empty l in
  List.map number_str ~f:Int.of_string

let to_record list =
  match list with
  | x1::x2::x3::[] -> {x1; x2; x3}
  (* Not having the 3 numbers is an error*)
  | _ -> assert false;;

let read_triplet () = match get_line () with
  | None -> [0;0;0]
  | Some x -> parse x

let a = to_record (read_triplet ())
let b = to_record (read_triplet ())

let grade x y =
  let f a b = if a > b then 1 else 0 in
  (f x.x1 y.x1) +
    (f x.x2 y.x2) +
    (f x.x3 y.x3)

let grade_a = grade a b
let grade_b = grade b a
let () = printf "%d %d" grade_a grade_b
