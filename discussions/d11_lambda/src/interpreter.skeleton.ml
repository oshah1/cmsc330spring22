open Parser

(* Evaluater *)

let rec eval (ast : expr) : int =
  match ast with
  | Int x -> x
  | Plus (x, y) -> let x1 = eval x in 
                    let y1 = eval y in
                    x1 + y1
  | Mult (x,y) -> let x1 = eval x in
                  let y1 = eval y in
                  x1 * y1
