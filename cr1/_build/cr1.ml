

(* Define an infix operator +/. that computes the average of two-floating point 
   numbers *)

let (+/.) x y = failwith "not yet implemented" ;; 


(* Define a function fib such that fib n is the nth number in the Fibonacci
sequence, which is 1, 1, 2, 3, 5, 8, 13... *)

let fib n = failwith "not yet implemented" ;; 


(* Define a function subset_sum that checks if there exists a subset of a
list with a given sum (called the target). *)
let subset_sum lst target = failwith "not yet implemented" ;; 


(* Define a function partition that divides a list into two lists, such that the
first list contains elements that satisfy a predicate function f and the second list contains elements
that don’t satisfy the predicate. You may assume the input list is an integer list. *)
let partition pred lst = failwith "not yet implemented" ;; 


(* Define a function pack that packs consecutive dupli-
cates of list elements into sublists. You may assume that the input lists are string lists. *)
let pack lst = failwith "not yet implemented" ;; 


(* 
Chess:
In chess, we move pieces across a 8x8 board such that the columns are labeled
as letters and the rows are labeled as numbers.

Positions on a chess board are referred to as a combination of the column letter and row number. For
instance, for the white pieces, we can say that the center pawn on the white square is at "e2".

Suppose, we want to define a type position that represents the position of a piece on the
board. What type expression is appropriate to represent a piece’s position? (note that int is just used 
a placeholder below so that this will compile)

*)
type position = int ;; 


(* After defining position, define a function count_position that takes in a position and
a position list, and counts the number of times the position occurs in the position
list. *)

let count_position pos lst = failwith "not yet implemented" ;; 


(* A knight in chess can move two squares vertically and one square horizontally, or two squares
horizontally and one square vertically (in an "L" shape). Define a function legal_knight_moves
that takes in a position and returns a position list representing all the possible posi-
tions the knight can move to. Hint: You may find the Char module helpful here. *)
let legal_knight_moves position = failwith "not yet implemented" ;; 


(* Define a function min_knight_steps that takes in a starting position s and a target
position t, and returns the minimum number of moves it takes for a knight to go from s or 6
t. Note, that a knight can visit any position on the board from any starting position, so there’s
always a solution! See the Knight’s tour. You may use the List.map function here. *)
let min_knight_steps s t = failwith "not yet implemented" ;; 