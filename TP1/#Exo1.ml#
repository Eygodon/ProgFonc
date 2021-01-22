#directory"../Modules/4.08.1";;
open List;;

let rec nb_of_5_mult (list : int list) : int =
  match list with
  | [] -> 0
  | a::lst -> if (a mod 5 = 0)
              then nb_of_5_mult(lst) + 1
              else nb_of_5_mult(lst)
;;

let rec pos_even_odd(l : 'a list) : 'a list * 'a list =
  match l with
  | [] -> ([], [])
  | e::[] -> (e::[], [])
  | e::f::lst ->
     let (le, lo) : ('a list * 'a list) = pos_even_odd(lst) in
     (e::le, f::lo)
;;

pos_even_odd([5 ; 2 ; 3 ; 7 ; 10]);;

(* Exercice 2*)

type 'a t_btree =
  |B_empty
  |B_node of 'a * 'a t_btree * 'a t_btree
;;

let t1  =
  B_node('b',
                B_node('h',
                       B_node('s',
                              B_node('q', B_empty, B_empty),
                              B_node('p', B_empty, B_empty)
                         ),
                       B_node('k', B_empty, B_empty)
                  ),
                  B_node('r',
                         B_node('c', B_empty, B_empty),
                         B_node('j',
                                B_node('o', B_empty, B_empty),
                                B_empty
                           )
                    )
    )
;;

let root (tree : 'a t_btree) : 'a =
  match tree with
  |B_empty -> invalid_arg("Tree is empty")
  |B_node(root, _, _) -> root
;;

    

let rec list_of_btree(t : 'a t_btree) : 'a list =
  match t with
  | B_empty -> []
  | B_node(r, ls, rs) -> (list_of_btree(ls) @(r::list_of_btree(rs)))
;;

let rec parcoursInfixeAux(a, res : 'a t_btree * 'a list) : 'a list =
  match a with
  |B_empty -> res
  |B_node(e,l,r) -> parcoursInfixeAux(l,e::parcoursInfixeAux(r,res))
;;

let parcoursInfixeStart(a : 'a t_btree) : 'a list =
  parcoursInfixeAux(a,[])
;;

let highest_each_branch( tree : 'a t_btree) : 'a list =
  highest_each_branch_aux(tree, root(tree))
;;

let rec highest_each_branch_aux(tree, elem: 'a t_btree * 'a ) : 'a list =
  match tree with
  | B_empty -> [elem]
  | B_node(root, B_empty, B_empty) -> (
    if (root > elem)
    then [root]
    else [elem]
  )
  |B_node(root, ls, rs) -> (
    if (root > elem)
    then (highest_each_branch_aux(ls, root)@highest_each_branch_aux(rs, root))
    else (highest_each_branch_aux(ls, elem)@highest_each_branch_aux(rs, elem))
  )
;;

let rec max_in_branch_aux(t, r_value, lst : 'a t_btree * 'a * 'a list) : 'a list =
    match t with
    | B_empty -> []
    | B_node(r, B_empty, B_empty) -> (max r_value r)::lst
    | B_node(r, ls, rs) ->
       let max_char : 'a = (max r_value r) in
       max_in_branch_aux(ls, max_char, max_in_branch_aux(rs, max_char, lst))
;;


let max_in_branch(t : 'a t_btree) : 'a list =
  match t with
  | B_empty -> []
  | B_node(r, ls, rs) -> max_in_branch_aux(ls, r, max_in_branch_aux(rs, r, []))
;;

max_in_branch(t1);;


highest_each_branch(t1);;           

type 'a t_tree3 =
  |B_node of 'a t_tree3 * 'a t_tree3
  |B_leaf of 'a         
;;
  
let t3 : int t_tree3 =
  B_node(
      B_node(
          B_node(B_leaf(5),
                 B_leaf(9)
            ),
          B_leaf(15)
        ),
      B_node(
          B_node(
              B_leaf(2),
              B_node(B_leaf(17), B_leaf(8))
            ),
          B_node(B_leaf(3), B_leaf(25))
        )
    )
;;

let rec leaf_count_aux(tree, list : 'a t_tree3 * 'a list): 'a list =
  match tree with
  |B_node(ls, rs) -> leaf_count_aux(ls, leaf_count_aux(rs, list))
  |B_leaf(valeur) -> valeur::list
;;

let leafList(tree : 'a t_tree3) : 'a list =
  leaf_count_aux(tree, [])
;;

leafList(t3);;

let rec leaf_Search_Aux(seeked, tree, list : 'a * 'a t_tree3* 'a list) : 'a list =
  match tree with
  |B_leaf(valeur) ->
    if (seeked = valeur)
    then (list)
    else ([])
  |B_node(ls, rs) -> leaf_count_aux(ls, 0::leaf_count_aux(rs, 1::list))
;;
