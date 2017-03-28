(* Creates a simple integer list of size n *)
open Core.Std
let make_array n = List.init n ~f:(fun x -> x)

let () =
    let n = int_of_string (read_line ()) in
    let arr = make_array n in
    List.iter ( Printf.printf "%d " ) arr
