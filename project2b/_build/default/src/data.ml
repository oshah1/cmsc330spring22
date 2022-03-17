open Funs
open Higher
(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

(*use pattern matching to check the integers in the tree
match with: (a,b,c,d,e) or (a,b,c,d)
if (a,b,c,d) then there is only 1 integer, so
   call int_insert on first leaf*)
let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode (x, None, IntLeaf, IntLeaf, IntLeaf)
  (*if there is only one integer present at the node. In this case,
    all branches are IntLeaf, so we just add x to this node
  First int must be smaller than second int, if x is less than
    a, x is the new first integer. Otherwise, a is the first*)
  | IntNode (a,None, b, c ,d) -> if x < a then IntNode (x, Some a, b, c, d)
                                      else if x > a then IntNode (a, Some x, b, c, d)
                                      else t
  | IntNode (a, Some b, c, d, e) -> if x < a then IntNode (a, Some b, int_insert x c, d, e) else 
                                            if x > a && x < b then
                                            IntNode(a, Some b, c, int_insert x d, e)  
                                            else if x > b then 
                                              IntNode (a, Some b, c ,d,int_insert x e) else t
  
  

let rec int_mem x t =
  match t with
   | IntLeaf -> false
   | IntNode (a,None, _, _ ,_) -> if x=a then true else false
   | IntNode (a, Some b, c, d, e) -> if x = a then true else if x = b then true 
                                            else if x < a then 
                                              int_mem x c 
                                            else if x > a && x < b then
                                             int_mem x d else int_mem x e
    

let rec int_size t =
  match t with
  | IntLeaf -> 0
  | IntNode (a, None, _ ,_ ,_) -> 1
  | IntNode (a, Some b, c, d, e) -> 2 + (int_size c) + (int_size d) + (int_size e)



let rec int_max t =
  
  match t with
  | IntLeaf -> invalid_arg "int_max"
  | IntNode (a, None, _, _, _) -> a
  | IntNode (a, Some b, _, _, e) -> try
                                      max a (max b (int_max e))
                                    with e -> max a b

(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_put k v t = 
  match t with
  | MapLeaf -> MapNode ((k,v), None, MapLeaf, MapLeaf, MapLeaf)
  | MapNode ((a, b), None, c, d, e) -> if k = a then
                                        invalid_arg "map_put"
                                         else if k < a then
                                        MapNode ((k,v), Some (a,b), c, d, e)
                                        else if k > a then
                                          MapNode ((a,b), Some (k, v), c, d, e)
                                        else
                                        invalid_arg "map_put"
  | MapNode ((a,av), Some (b,bv), c, d, e) -> 
                                      if k = a || k =b then
                                      invalid_arg "map_put"
                                       else if k < a then
                                        MapNode ((a, av), Some (b, bv), (map_put k v c), d, e)
                                        else if k > a && k < b then
                                          MapNode ( (a,av), Some (b,bv), c, map_put k v d, e)
                                        else if k > b then
                                          MapNode ((a,av), Some (b,bv), c, d, map_put k v e)
                                        else
                                        invalid_arg "map_put"

let rec map_contains k t = 
  match t with
  | MapLeaf -> false
  | MapNode ((a, b), None, c, d, e) -> if k < a then map_contains k c else if k = a then
                                        true else
                                            false
  | MapNode ((ak,av), Some (bk,bv), c, d, e) -> if k = ak || k = bk then
                                                true
                                              else if k < ak then
                                                map_contains k c
                                              else if k > ak && k < bk then
                                                map_contains k d
                                              else if k > bk then
                                                map_contains k e
                                                else
                                                invalid_arg "map_contains"

let rec map_get k t =
  match t with
  | MapLeaf -> invalid_arg "map_get"
  | MapNode ((a, b), None, c, d, e) -> if k = a then b else if k < a then
                                        map_get k c else invalid_arg "map_get"
  | MapNode ((ak,av), Some (bk,bv), c, d, e) -> if k = ak then av 
                                                else if k = bk then bv 
                                                else if k < ak then
                                                  map_get k c
                                                else if k > ak && k < bk then
                                                  map_get k d else 
                                                    map_get k e


(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
(*Acts sort of like a linked list
tuple containing a list of string * int tuple and a lookup_table object, which is
the previous scope*)
type lookup_table =
| TailTable
| HeadTable of ((string * int) list) * lookup_table

let empty_table : lookup_table = TailTable

let push_scope (table : lookup_table) : lookup_table =
  match table with
  | TailTable -> HeadTable ([], table)
  | HeadTable (_, _) -> HeadTable ([],table)
  

let pop_scope (table : lookup_table) : lookup_table =
  match table with
  | TailTable -> failwith "No scopes remain!"
  | HeadTable (_, x) -> x

let add_var name value (table : lookup_table) : lookup_table =
  let new_var = (name, value) in
  match table with
  | TailTable -> failwith "There are no scopes to add a variable to!"
  | HeadTable (lst, x) -> if contains_elem lst new_var then failwith "Duplicate variable binding in scope!"
                                            else HeadTable ((new_var::lst),x)

let rec search_elem lst name=
  match lst with
  | [] -> failwith "Variable not found!"
  | (a,b)::t -> if a = name then b else search_elem t name

let rec lookup name (table : lookup_table) =
  match table with
  | TailTable -> failwith "No Scope!"
  | HeadTable (lst,t)-> search_elem lst name