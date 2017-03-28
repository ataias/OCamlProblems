(* Gets length of a list, tail recursively, without 
   the standard libraries *)

(* This function was given by HackerRank *)
let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

(* Sum only odd numbers *)
let len lst =
  let rec length n = function
    | [] -> n
    | hd::tl -> length (n+1) tl in
  length 0 lst
  

(* This function was given by HackerRank *)
let () =
  let ans = len (read_lines ()) in
  print_int ans; 
  print_newline ()

