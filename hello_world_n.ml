(* Reader n and prints hello world n times *)
open Core.Std
let n = read_int ();;

let rec print_hello n =
  if n > 0 then begin
      printf "Hello World\n";
      print_hello (n-1);
    end else ();;

print_hello n
