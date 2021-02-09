#directory"../Modules/4.08.1";;
open List;;
#load "btree.cmo";;
open Btree;;
map;;

let rec mapList2 op lst1 lst2 =
  match (lst1, lst2) with
  | ([], []) -> []
  | (e1::lt1, e2::lt2) -> (op e1 e2)::mapList2 op lt1 lt2
;;

mapList2 + [5;6;3] [6;3;4];;

let max_map2 = map2(fun e1 e2 -> max e1 e2);;
max_map2 [5;3;4] [10;15;12];;
let mapmax2 = map2 max;;
mapmax2 [12;35;21] [51;0;1];;
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
let vect_scal v = List.map  ( ( * ) v);;
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

(*
let rec gen_tree(generator, nb_nodes : (unit -> 'a) * int) : 'a tree =
    if nb_nodes <= 0
    then Nil
    else 
        let nb_nodes_left = Random.int nb_nodes in
        Node(generator(), gen_tree(generator, nb_nodes_left), gen_tree(generator, nb_nodes - nb_nodes_left - 1))
;;

let gen1 min max = (Random.int (max - min)) + min;;

let generate10_20 : (unit -> int) = (fun unit -> (gen1 10 20));;

let tree = gen_tree(generate10_20, 10)

let generateA_B : (unit -> char) = (fun unit -> Char.chr(gen1 97 122));;

let tree = gen_tree(generateA_B, 10);;
 *)

let rec rndTree (randV : unit -> 'a) (nbNodes : int) : 'a t_btree =
  if(nbNodes = 0)
  then empty()

  else
    let value : 'a = randV() and sep : int = Random.int nbNodes in
      rooting(value, rndTree randV sep, rndTree randV (nbNodes-sep-1))
;;

show_int_btree(rndTree (fun unit -> Random.int 100) 20);;

let rndTreeBounded (min : 'a) (max : 'a) =
  rndTree (fun unit -> (Random.int (max-min)) + min)
;;

show_int_btree(rndTreeBounded 5 20 20);;


let rndTreeBoundedChar (min : 'a) (max : 'a) =
  rndTree (fun unit -> ( String.make 1 (Char.chr(Random.int(max-min) + min))))
;;

show_string_btree(rndTreeBoundedChar 97 122 20);;

(* Exercice 7 *)

let non = (not) ;;
let et  = (&&);;
let ou  = (||);;
ou (5=3) (3=8);;
let pair nb = (nb mod 2) = 0;;
