(* Print pascal triangle up to kth line*)
let k = read_int();;
let rec f = function
  | 0 | 1 -> 1
  | n -> n * f (n - 1)

let el n r = (f n) / ((f r) * (f (n - r)))
           
let rec print_line n r =
  if r <= n then begin
        print_int (el n r);
        print_string " ";
        print_line n (r + 1);
      end else print_newline ();;

(* Start and end lines*)
let rec pascal s e =
  if s < e then begin
      print_line s 0;
      pascal (s + 1) e;
    end


let () = pascal 0 k
