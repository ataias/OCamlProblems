(*
* Given a time in 12-hour AM/PM format, convert it to military (24-hour) time.
* https://www.hackerrank.com/challenges/time-conversion
*)
open Core.Std;;

let get_list_from_time_12 t =
    let noSuffix = String.drop_suffix t 2 in
    let l = String.split noSuffix ~on:':' in
    let time_type = String.suffix t 2 in 
    l, time_type

let convert_hour l time_type =
    let prepend tl hd = hd::tl in
    let conv x = if x <> 12 then x + 12 else x in
    let zero_pad x = if String.length x = 1 then 
        String.concat ["0"; x] else x in
    match l with
    | [] -> failwith "Empty time"
    | h::tl -> if time_type = "PM" then 
        Int.of_string h 
        |> conv
        |> Int.to_string
        |> zero_pad
        |> prepend tl
        else (if h = "12" then "00" else h)::tl

let convert t =
    let l, time_type = get_list_from_time_12 t in
    convert_hour l time_type
    |> String.concat ~sep:":"

(* IO *)
let time = read_line();;
let () = print_string (convert time) in
print_newline()
