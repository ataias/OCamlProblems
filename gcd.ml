(* Input is a single line with two integer numbers 
   The output is a single line with the greatest 
	 common divisor *)
open Core.Std
type numbers = {
    a : int;
    b : int;
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
  | a::b::[] -> {a; b}
  (* Not having the 3 numbers is an error*)
  | _ -> assert false;;

let read_pair () = match get_line () with
  | None -> [0;0]
  | Some x -> parse x

let n = to_record (read_pair ())

let rec gcd a = function
  | 0 -> a
  | b -> gcd b (a mod b)

let () = 
  print_int (gcd n.a n.b);;
  print_endline;;
  
