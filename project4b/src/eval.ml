open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder map ping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable x" ))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
   let match_int (a: value ) (b:value) : (int * int) =
    match a,b with
    | Int x, Int y -> (x,y)
    | _, _ -> raise (TypeError "Expected type int")

let rec eval_expr env e = 
  match e with
  | (Value x) -> begin
                    match x with
                    | Int y -> Int y
                    | Bool b -> Bool b
                    | String s -> String s
                    | Closure (env, var, exp) -> Closure (env, var, exp)
                    
                    end
  | (ID id) -> lookup env id
  | (Not (ID x)) -> let v = lookup env x in
                        begin match v with
                        | Bool b -> Bool (not b)
                        | _ -> raise (TypeError "Eval not") end
  | (Not x)-> let value = eval_expr env x in begin
                  match value with
                  Bool b -> Bool (b <> true)
                  | _ -> raise (TypeError "Expected type bool") end

  (*Handles all binary operations*)
  | (Binop (o, x,y)) -> let x1 = eval_expr env x in
                        let y1 = eval_expr env y in
                        begin
                        match o with
                        | Add -> let (a,b) = match_int x1 y1 in Int (a+b)
                        | Sub -> let (a,b) = match_int x1 y1 in Int (a-b)
                        | Mult -> let (a,b) = match_int x1 y1 in Int (a*b)
                        | Div -> begin match x1, y1 with
                                  | Int _, Int 0 -> raise DivByZeroError
                                  | Int a, Int b -> Int (a/b)
                                  | _ , _ -> raise (TypeError "Expected type Int") end
                        | Greater -> let (a,b) = match_int x1 y1 in Bool (a > b)
                        | Less -> let (a,b) = match_int x1 y1 in Bool (a < b)
                        | GreaterEqual -> let (a,b) = match_int x1 y1 in Bool (a >= b)
                        | LessEqual -> let (a,b) = match_int x1 y1 in Bool (a <= b)
                        | Concat -> begin match x1, y1 with
                                    | String a, String b -> String (a ^ b)
                                    | _, _ -> raise (TypeError "Expected type String") end
                        | Equal -> begin match x1, y1 with
                                    | Int a, Int b -> Bool (a=b)
                                    | Bool a, Bool b -> Bool (a=b)
                                    | String a, String b -> Bool (a=b)
                                    | Closure (_,_,_), Closure (_,_,_) -> raise (TypeError "Cannot compare Closures")
                                    | _,_ -> raise (TypeError "Cannot compare types") end
                        | NotEqual -> begin match x1, y1 with
                                | Int a, Int b -> Bool (a<>b)
                                | Bool a, Bool b -> Bool (a<>b)
                                | String a, String b -> Bool (a<>b)
                                | Closure (_,_,_), Closure (_,_,_) -> raise (TypeError "Cannot compare Closures")
                                | _,_ -> raise (TypeError "Cannot compare types") end
                        | And -> begin match x1,y1 with
                                  | Bool a, Bool b -> Bool (a&&b)
                                  | _, _ -> raise (TypeError "Expected type Bool") end
                        | Or -> begin match x1,y1 with
                                  | Bool a, Bool b -> Bool (a || b)
                                  | _, _ -> raise (TypeError "Expected type Bool") end
                        end
    (*evaluate guard branch and check if it evaluates to a bool*)
  | If (a,b,c) -> begin let guard = eval_expr env a in
    
      (*if guard evals to true, evaluate true branch, else
        evaluate false branch*)
      match guard with 
      | Bool x -> if x then eval_expr env b else eval_expr env c
      | _ -> raise (TypeError "Expected type bool") end
  | (Let (name, recurse, init, body)) -> if recurse then
                                            (*create environment that maps name to a
                                            temporary placeholder*)
                                            let temp = extend_tmp env name in
                                            (*evaluate initialization*)
                                            let exp = eval_expr temp init in
                                            (*map evaluated initialization to name*)
                                            let () = update temp name exp in
                                            eval_expr temp body
                                          else
                                            (*evalue initialization expression*)
                                            let exp = eval_expr env init in

                                            (*eval body*)
                                            eval_expr (extend env name exp) body

  | Fun (a, b) -> (*capture current environment*) Closure (env, a, b)

  | FunctionCall (a,b) -> let exp1 = eval_expr env a in begin 
                          match exp1 with 
                          | Closure (e, name, body) -> let v1 = eval_expr env b (*eval e2*)in
                          (*extend closure's enironment with parameter name bound to v1*)
                                                        let a' = extend e name v1 in
                                                        eval_expr a' body
                          | _ -> raise (TypeError "Expected closure") end

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with
  | Def (v, exp) -> let temp = extend_tmp env v in (*set placeholder for v*)
                    let exp1 = eval_expr temp exp in
                    update temp v exp1; (*update binding for v*)
                    (temp, Some exp1)
  | Expr e -> let exp = eval_expr env e in
              (env, Some exp)
  | NoOp -> (env, None)

