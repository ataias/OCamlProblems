(* The input is a sequence of integers, one in each line
   The output is the sum of the odd ones *)
(* This function was given by HackerRank *)
let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

(* Sum only odd numbers *)
let f lst =
  let rec g lst sum = match lst with
    | [] -> sum
    | hd::tl ->
       let add = if abs(hd mod 2) = 1 then hd else 0 in
       g tl (sum + add) in
  g lst 0
  

(* This function was given by HackerRank *)
let () =
  let ans = f (read_lines ()) in
  print_int ans; 
  print_newline ()

