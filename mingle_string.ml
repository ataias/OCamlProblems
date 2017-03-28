(* More information: https://ataias.com.br/2017/03/26/mingle-strings-in-ocaml/*)
(* example:  
   ### input
   abc
   xyz
   ### output
   axbycz
*)
open Core.Std;;
(* Read the two input strings *)
let a = read_line();;
let b = read_line();;

let mingle a b =
  let la = String.length a in
  let lb = String.length b in
  let total = la + lb in
  let s = String.create total in
  let rec m i =
    (* j = index for inputs *)
    let j = i / 2 in
    if i > (total - 1) then s
    else let () =
      (* i = index for output *)
      s.[i] <- a.[j];
      s.[i+1] <- b.[j];
      in m (i + 2); in
  m 0;;

let () = print_string (mingle a b) in
print_newline()
