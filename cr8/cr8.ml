

(* Problem 1 *)


(* Problem 2 *)

class type polynomial_type = 
 object 

  (* returns the coefficients of the polynomial in order from lowest degree to highest
     degree *)
  method get_coefficients : float list 
  
  (* `evaluate` x evalutes a polynomial f at x, i.e. computes f(x) *)
  method evaluate : float -> float 

  (* solve c returns real solutions for x when a polynomial equals c. In other words, 
     it should return the real solution solutions x such that f(x) = c *)
  method solve : float -> float list option 

 end ;; 

class polynomial (coefficients : float list) : polynomial_type = 
 object 
  method get_coefficients : float list = coefficients 
  method evaluate (x : float) : float = failwith "not yet implemented" 
  method solve (c : float) : float list option = None 
 end ;; 

