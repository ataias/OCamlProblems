(* Simple usage of a map *)
open Core.Std

let m = Map.empty (String.comparator);;
let m = Map.add m ~key:"oi" ~data:"tchau";;

Map.find m "oi";;
