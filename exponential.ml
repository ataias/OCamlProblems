(* Reads n cases from stdin and outputs the exponential of them *)
let rec fact n acc =
  if n <= 1.0 then acc else fact (n -. 1.) (acc *. n)

let exp x =
  let rec e n acc =
    if n > 9.0 then
      acc
    else let delta = (x ** n) /. (fact n 1.0) in
         e (n +. 1.) (acc +. delta)
  in e 0.0 0.0


let rec read_lines n = 
  try if n > 0 then
        let number = read_float () in
        number :: read_lines (n-1)
      else []
    with
        End_of_file -> []
            
let () =
    let n = read_int () in
    let cases = read_lines n in
    let output = List.map exp cases in
    List.iter (
      fun x -> print_float x; 
      print_newline ()) output;;

