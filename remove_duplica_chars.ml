(* Remove duplicate chars in a string 
   Example: 
	 input: abcxxyyabc
	 output: abcxy 
	 
	 How to compile:
	 corebuild remove_duplica_chars.native *)
open Core.Std
(* string -> char list *)
let explode str =
  let len = String.length str in
  let rec expl str i =
    if i > len - 1 then [] else
      str.[i]::(expl str (i+1)) in
  expl str 0

(* char list -> string *)
let implode clist =
  let len = List.length clist in
  let str = String.create len in
  let rec impl i = function
    | [] -> str
    | c :: l -> str.[i] <- c; impl (i+1) l in
  impl 0 clist

(* string -> string *)
let remove_all_dups str =
  let char_list = explode str in
  let s = Set.empty ~comparator:Char.comparator in
  let rec remove s no_dup_l = function
    | [] -> no_dup_l
    | c :: t ->
       let f = fun x -> x = c in
       let found = Set.exists s ~f:f in
       if found then
         remove s no_dup_l t
       else
         let s = Set.add s c in
         remove s (c :: no_dup_l) t in
  remove s [] char_list
  |> List.rev |> implode

let a = read_line();;
let () = print_string (remove_all_dups a) in
print_newline()
