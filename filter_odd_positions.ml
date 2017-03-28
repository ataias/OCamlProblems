(* This will read numbers into a list and remove the odd positions *)
(* This function was given by HackerRank *)
let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

(* I filled this function *)
let f lst =
  let rec no_odd lst is_odd =
    match lst with
    | [] -> []
    | hd::tl -> if is_odd then no_odd tl false else hd::(no_odd tl true) in
  no_odd lst true
  

(* This function was given by HackerRank *)
let () =
  let arr = read_lines() in
  let ans = f arr in
    List.iter (
      fun x -> print_int x; 
      print_newline ()) ans;;
