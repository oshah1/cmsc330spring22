(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = 
  match tup with
  | (a, b, c) -> (c, b, a)

let is_odd x = 
  if (x mod 2) = 0 then
    false
  else
    true


let get_x tup =
  match tup with
  | (a, b) -> a
  

let get_y tup =
  match tup with
  | (a,b) -> b
  


let get_z tup = 
  match tup with
  | (a, b, c) -> c

let area x y = (abs ((get_x x) - (get_x y) )) * abs ((get_y x) - (get_y y))

let volume x y = 
  match x with
  | (ax,bx,cx) -> (match y with
                | (ay,by,cy) -> (area (ax, bx) (ay,by) ) * (abs ((get_z x) - (get_z y))) )

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = 
  if n = 0 then
    0
  else if n = 1 then
    1
  else 
    (fibonacci (n-1)) + (fibonacci (n-2))

let rec pow x y = 
  if y = 0 then
    1
  else
    x * (pow x (y-1))

let rec log x y = 
  if (y/x) < x then
    1
  else
    1 + (log x (y/x))

  
let rec gcf_aux x y div =
  if div < 0 then
    0
else if ((x mod div = 0) && (y mod div =0)) then
  div
else
  gcf_aux x y (div - 1);;

let rec gcf x y = 
  if x = y then
    y
  else if x = 0  then
    y
  else if y = 0 then
    x
else
 gcf_aux x y y

let rec is_prime_aux x div =
  if div = 2 then
    (x mod div != 0)
  else if x mod div = 0 then (*if x is divisible by anything other than 1 or itself*)
    false
  else
    is_prime_aux x (div - 1)


let rec is_prime x = 
  if x < 0 then
     false
  else if x = 0 then
    false
  else if x = 1 then
    false
  else if x = 2 then
    true
  else
    is_prime_aux x (x-1)

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = 
  match lst with
  | [] -> failwith "Out of bounds"
  | h::t -> if idx = 0 then h
            else get (idx -1) t 

let rec length lst =
  match lst with
  | [] -> 0
  | _::t -> 1 + length t


let larger lst1 lst2 = 
  let len1 = length lst1 in
  let len2 = length lst2 in
  (if len1 < len2 then lst2 else if len2 < len1 then
    lst1 else [] ) 

(*function takes two parameters: the list to reverse the elements of 
and a list to prepend elements to*)
let rec reverse_aux lst rev =
  (*base case: lst is empty*)
  (*otherwise: lst has elements in it*)
  match lst with
  | [] -> rev
  | h::t -> reverse_aux t (h::rev) (*prepend head element to rev, pass tail*)


let reverse lst =
  reverse_aux lst []



(*Try reversing the first list, then repeatedly add its elements
to lst1*)
let rec combine lst1 lst2 = 
  (*reverse both lst1 to get the lead element*)
  let lst1_rev = reverse lst1 in
  match lst1_rev with
  | [] -> lst2
  | h::t -> combine (reverse t) (h::lst2)
  (*base case: one or both lists are empty*)
  

(*list will end up in reverse sorted order when done,
just call reverse on output*)
let rec merge_aux lst1 lst2 output =
  match lst1 with
  | [] -> (match lst2 with
            | [] -> reverse (output)
            | h::t -> merge_aux [] t (h::output))

  | h1::t1 -> (match lst2 with
              [] -> merge_aux t1 [] (h1::output)
              | h2::t2 -> if h1 < h2 then
                          merge_aux t1 lst2 (h1::output) else if h1 = h2 then
                            merge_aux t1 t2 (h1::(h2::output)) else
                              merge_aux lst1 t2 (h2::output))


let rec merge lst1 lst2 = 
  match lst1 with
  | [] -> lst2
  | _::_ -> merge_aux lst1 lst2 []


  (*use pattern matching to extract the head of the list,
  reverse the tail, then prepend the head to the reversed list, then
    reverse the list again*)
let rec rotate shift lst = 
  if shift = 0 then
    lst
  else
    match lst with
    | [] -> lst
    | h::t -> rotate (shift -1) (reverse (h::reverse t))


let rec list_same lst1 revlist =
  match lst1 with
  [] -> true
  | h::t -> (match revlist with
            | [] -> true
            | h2::t2 -> if h = h2 then
              list_same t t2 
            else
                false)

    (*first, reverse the list, compare list tail and head, if they are equal. 
      call is_palindrome, passing tail as parameter*)
let rec is_palindrome lst = 
  let revlist = reverse lst in match lst with
  | [] -> true
  | [x] -> true
  | h::t -> list_same lst revlist
 