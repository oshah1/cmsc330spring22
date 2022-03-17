open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

(*sets accumulator to "true" if an element is found*)
let contains_elem lst e = 
  fold (fun a x -> if x=e then true else a) false lst

let is_present lst x = 
  map (fun e -> if e = x then 1 else 0) lst

let count_occ lst target = 
  fold (fun a x -> if x=target then (a+1) else a) 0 lst

    (*pass a function that checks if a given element exists in
      a list. if it doesn't add that element to the list. Other wise, keep the
        list as is, the lst to operate on, and an empty list for the accumulator*)
let uniq lst = 
  fold_right (fun x a -> if (contains_elem a x) then a else x::a) lst []

(* int * int -> int * int list -> bool*)
let rec find_tuple x lst =
  match lst with
  | [] -> false
  | h::t -> match h with
            | (a, b) -> if a = x then true else find_tuple x t

    
let inc_tuple tup =
  match tup with
  | (a, b) -> let c = b+1 in (a,c)

(*accumulator will be a list of pairs
operation on head of list: if a tuple with the head as its first element 
  already exists in the accumulator, increment that tuple by 1 and prepend the new tuple to the list*)
(*if the first digit of a tuple is equal to x (an integer in lst) increment the
                                                tuple. otherwise, leave the tuple as is*)
let assoc_list lst = 
  fold (fun a x -> if (find_tuple x a) = true then 
                                            map (fun tup -> match tup with
                                                    | (b,c) -> if b=x then
                                                    (inc_tuple tup) else (b, c)) a else ((x,1)::a )) [] lst

(*initially, accumulator is going to be an empty list
fold right thru fns, map head function to every item in args. function
returns a list of modified elements as result Then,
fold again, appending every element in list of results to a new list*)
let ap fns args = 
  let new_list func=
    map func args in
  (*x is a function from fns, acc is the accumulator we passed into fold*)
  (*inner fold: map prepend from given list to another list, list is map result,
  accumulator is acc, from outer function*)
  fold_right (fun x acc -> fold_right (fun h lst -> h::lst) (new_list x) acc) fns [] 
