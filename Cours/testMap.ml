
open List;;
open Btree;;
map(fun nb -> nb * nb) [1; 2; 3; 4];;
map(fun s -> String.get s 0) ["bonjour" ; "lol"];;
fold_left( * ) 1 [1 ; 2 ; 3 ;4];;
fold_left(fun _ cpt -> cpt+1) 0 [1 ;48 ;15 ;3];;
fold_left( fun b v -> b || v = 0 ) false [0; 1; 3 ;5];;
let rec tree_map func the_tree = match the_tree with
    Leaf(n) -> Leaf(func n)
  | Node(left_tree, right_tree) -> Node(tree_map func left_tree, tree_map func right_tree)
;;
let a = (Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4))));;

tree_map( ( * )2 ) a;;

tree_fold_infix_left (fun elem root -> elem == root) 0 (Node(1, Node(0, Nil, Nil), Node(3, Nil, Nil)));;

let search elem the_tree = tree_fold_infix_left (func b v -> b || v = elem) false the_tree ;;

let count elem the_tree = tree_fold_infix_left ( func res curr -> if curr = elem then  res+1 else res) 0 the_tree;; 
