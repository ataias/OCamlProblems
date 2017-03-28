(* Reads first a line with T, the number of test cases*)
(* Later reader T lines with numbers "a b" and outputs 
   the number of common divisors for each of those *)
open Core.Std

(* Need to check divisors just up to sqrt x *)
let max x = Int.of_float (sqrt x) |> (+) 1;;

let get_divisors n =
  let c = Int.comparator in
  (* Cap is the limit for the iterations *)
  (* This is sqrt(n) *)
  let cap = max (Float.of_int n) in
  let set = Set.empty ~comparator:c in
  let rec divs set i =
    if n mod i = 0 then
      let set = Set.add set i in
      divs set (i+1)
    else
      if i > cap then set else divs set (i+1)
  (* This set does not have all divisors 
     As the pairs are missing*)
  in let set = divs set 1 in
  (* Add the missing pairs *)
  let f s x = Set.add s (n / x) in
  Set.fold set ~init:set ~f:f;;

let get_number_common_divisors n m =
  let divs_n = get_divisors n in
  let divs_m = get_divisors m in
  let common = Set.inter divs_n divs_m in
  Set.length common;;


(* IO Code *)

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
  
let n_tests = read_int ();;
let () = for i = 1 to n_tests do
    let line = read_line () in
    let l = parse line in
    let a, b = match l with
      | x::y::tl -> x, y
      | _ -> 0, 0 in
    print_int (get_number_common_divisors a b);
    print_newline ();
done
