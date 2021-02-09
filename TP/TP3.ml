open List;;

(* Exercice 1 *)
let rec multList(list : int list) : int list =
  match list with
  | [] -> list
  | elem::tail -> (2*elem)::multList(tail)
;;

let multListMap = List.map (( * ) 2);;

multList([5;3;9]);;

let rec capitrec (list : 'a list) : 'a list =
  match list with
  | [] -> list
  | elem::tail -> String.capitalize_ascii(elem)::capitrec(tail)
;;

let capitrecMap = List.map (fun e1 -> String.capitalize_ascii(e1));;

capitrec(["louis" ; "yann"]);;

let rec isAllLetters (l : char list) : bool =
  match l with
  | [] -> true
  | e::tail -> (e >= 'a' && e <= 'z' && e >= 'A' && e <= 'Z') && isAllLetters(tail)
;;

let is_letter e = ((e >= 'a' && e <= 'z') || (e >= 'A' && e <= 'Z'));;

let isAllLetters_fl = List.fold_left( fun e -> is_letter ) true;;

isAllLetters_fl ['1'; 'e'];;

let rec sumOfEvens (list : int list) : int =
  match list with
  | [] -> 0
  | e::tail ->
     if e mod 2 = 0
     then e + sumOfEvens(tail)
     else sumOfEvens(tail)
;;

let sumOfEvens_fl = List.fold_left (fun e -> if e mod 2 = 0 then (+) e else (+) 0) 0;;

let rec countNumbers ( list, res: 'a list* int) : int =
  match list with
  | [] -> res
  | e::tail ->
     if (e >= '0' && e <= '9')
     then countNumbers(tail, res+1)
     else countNumbers(tail, res)
;;

countNumbers (['g'; '9'; 'p'; 'd'], 0);;

let rex max_length_of_string_list (stringlist : string list) : int =
  match stringlist with
  | [] -> 0
  | e::tail -> max (String.length e) (max_length_of_string_list(tail))
;;

                                      
