
type 'a mlist = 
  'a mlist_internal ref 
  and 
    'a mlist_internal = 
      | Nil 
      | Cons of 'a * 'a mlist ;; 

(* 
Define a function mem_factorial which takes a unit as an argument and returns 1 the first
time it is called, returns 2 the second time it is called, returns 6 the third time it is called, and
so on. In general, the function should return n! on the nth time it is called. 
*)


let mem_factorial : unit -> int = 
   let n = ref 1 in 
   let fact = ref 1 in 
    fun () : int -> 
      n := !n + 1; 
      fact := !n * !fact; 
      !fact / (!n) ;;


(* Define a non-recursive implementation of List.filter *)
let filter_imper _ = failwith "not implemented" ;; 

(* range n m returns a list that contains all the integers from n to m-1 inclusive. Define three
versions of the range function: (1) a tail-recursive version, (2) a non-tail recursive version,
and (3) a procedural implementation (using loops) *)

let range _ = failwith "not implemented" ;; 

(* Define a function that merges two sorted (in ascending order) mutable lists. This function
should return a value of type unit and the first list should become the merging of the two lists *)
let rec merge (x : 'a mlist) (y : 'a mlist) : unit = 
  match !x, !y with 
  | _, Nil -> () 
  | Nil, _ -> x := !y 
  | Cons (hx, tx), Cons (hy, ty) -> 
    if hx < hy then merge tx y 
    else (merge x ty ; x := Cons(hy, x)) ;; 
 ;; 

(* Define a function that generates the Fibonacci numbers (i.e. fib(i) the ith time the function
is called *)
let fib _ = failwith "not implemented" ;;


(* Rewrite power from Part 2 (see Problem 6) to improve the worst-case time complexity. 
What is the asymptotic runtime of the function right now? *)
let rec power (x : int) (n : int) : int =
  if n = 0 then 1
  else 
    let power_n2 = power x (n / 2) in 
    if n mod 2 = 0 then power_n2 * power_n2 
    else power_n2 * power_n2 * x ;; 
