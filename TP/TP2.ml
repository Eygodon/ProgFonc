#directory"../Modules/4.08.1";;
open List;;
map;;

let rec mapList2 op lst1 lst2 =
  match (lst1, lst2) with
  | ([], []) -> []
  | (e1::lt1, e2::lt2) -> (op e1 e2)::mapList2 op lt1 lt2
;;

mapList2 + [5;6;3] [6;3;4];;

let max_map2 = map2(fun e1 e2 -> max e1 e2);;

(* Exercice 2*)

let incre  = (+) 1;;
let inver  = (/.) 1.;;
let prefin = (^) "in";;
let add2elem e1 e2  = (@)[e1 ; e2];;

let l : int list = add2elem 5 3 [3 ; 2 ; 1];;

(* Exercie 3*)

(*
let inverVect = List.map (fun e -> e * -1);;
let vect_scal l = List.map (fun e -> e * l);;
let vect_add = List.map2 (fun e1 e2 -> e1 + e2);;
let vect = [1 ; 2; 3];;
*)
let inver_vect = List.map  ( ( * ) (-1));;
let vect_add v = List.map  ( ( * ) v);;
let vect_add   = List.map2 ( + );; 
inverVect vect;;
vect_scal 5 vect;;
vect_add vect vect;;

(* Exercice 5 *)

type 'a expression =
  | Cst of 'a
  | Var of char
  | Op  of ('a -> 'a -> 'a) * 'a expression * 'a expression
;;

let rec eval(exp : 'a expression) = 
  match exp with 
  | Cst(v) -> Cst(v)
  | Var(ch) -> Var(ch)
  | Op(o, o1, o2) ->
     let e = (eval(o1), eval(o2)) in
     match e with
     | (Cst(v1) , Cst(v2)) -> eval(Cst(o v1 v2))
     |_ -> exp
;;

(* Exercie 6 *)

type 'a tree =
  | Nil
  | Node of 'a * 'a tree * 'a tree
;;

let rec create_tree (gen, nb_node : 'a * int) : 'a tree =
  if nb_node <= 0
  then Nil
  else Node(gen, create_tree(gen, nb_node - 1), create_tree(gen, nb_node - 2))
         ;;
