(* Fibonnaci *)
(* This could be better if implemented with 
   dynamic programming *)
let n = read_int();;

let rec fib = function
  | 1 -> 0
  | 2 -> 1
  | x -> fib (x - 1) + fib (x - 2)
  
let () =
  print_int (fib n);
  print_newline ();
