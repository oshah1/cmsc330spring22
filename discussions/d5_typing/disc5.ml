(* Typing Practice  *)


1) let f x y = x + y
int -> int -> int

x +. y
float -> float -> float

2) let f x y = [x; y]
 'a -> 'a -> 'a list

3) 
let f a = 
  if a then 1 else "hi"

INVALID

4) 
let f a b = 
  if a then 0 else b

bool -> int -> int

5) 
let f a b c = 
  let d = "hi" ^ a ^ b ^ c in 
    d == "hello"

String -> String -> String -> bool

(* Creating functions *)


1) float -> float -> float

let f a b -> a +. b

2) 'a -> 'b -> 'b list

let f a b = if true then [b] else []

3) int -> float -> int

let f a b -> a + (int_of_float b)
let f a b = if b = 0.4 then a else 3

4) 'a -> 'a -> 'a list

let f a b = [a; b]

(* Records and New Types  *)


1) 
type age = (str * int)

let a: age = ("Vyoma", 8)

let a: hello = ("a",5)


2) 
type Coin = Heads | Tails

let b: Coin = Heads

let f coin =
  match coin with
  | Heads ->
  | Tails ->

3) 
type date = {month: string, day: int}

let today = {day=16; month="June"}

today.day

4)
type Shape = Rect (float * float)
        | Circle float

let f shape =
  match shape with
  | Rect (a,b) -> a * b
  | Circle rad -> rad * rad * pi