(* THIS MUST BE RUNNED WITH -rectypes *)
(* Combinators *)

(* I Combinator *)
let cI x = x

(* K Combinator *)
let cK x y = x

(* S Combinator *)
let cS x = fun y z -> x z (y z)


(* Fixed-point Combinator *)

(* U Combinator *)
let cU f = f f

(* U Mutual Combinator *)
let mcU l = List.map (fun x -> x l) l

(* Y Combinator *)
(* 0. Y f = f (Y f) *)
let rec cY_r f = f (cY_r f)

(* 1. without let rec *)
let cY = cU (fun y f a -> f (y y f) a)

(* 2. shorter *)
let cY_s f = cU (fun y a -> f (y y) a)

(* Y Mutual Combinator *)
(* 1. ? *)
let mcY l = cU (fun y ->
  List.map (fun f a -> f (y y) a) l)

(* Z Combinator *)
let cZ f = cU (fun z -> f (fun v -> z z v))
  

let fact = cZ (fun f -> function
  0 -> 1
  | n -> n * f (n - 1)
)

let _ = print_int (fact 10)
  


