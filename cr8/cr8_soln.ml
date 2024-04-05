

(* Problem 1 *)
class type user_type = 
 object 

  method get_username : string 

  method get_id : string 

  method add_friend : user_type -> unit

  method get_friends : user_type list 

  method add_post : string -> unit
  
  method remove_post : string -> unit 

  method get_posts : string list 

 end ;; 

 class user (username : string) (id : string) : user_type = 
  object (this)

    val mutable friends : user_type list = [] 
    val mutable posts : string list = [] 

    method get_username = username 

    method get_id = id 

    method add_friend (friend : user_type) : unit = 
        if 
          List.mem friend friends 
        then 
          () 
        else  
          (friends <- friend :: friends; 
          friend#add_friend (this :> user_type))
    
   method get_friends = friends 

   method add_post post = posts <- post :: posts 

   method remove_post post = posts <- List.filter ((<>) post) posts 

   method get_posts = posts 
        
  end ;; 

class type student_type = 
 object 
 inherit user_type 
 method get_school : string 

 end ;; 

class student (username : string) (id : string) (school : string) = 
 object 

 inherit user username id 
 method get_school = school 

 end ;; 


let rec form_friend_group (users : user_type list) : unit = 
  match users with 
  | [] -> () 
  | head :: tail -> 
      List.iter (fun user -> head#add_friend user) tail; 
      form_friend_group tail ;; 

let jayden = new student "jaydenp" "1" "Harvard" ;; 
let gerson = new student "gersonp" "2" "Harvard" ;; 
let kwee = new student "kwee" "3" "Harvard" ;; 
let victoria = new student "victoria" "4" "Harvard" ;; 

let user_list : student list = [jayden; gerson; kwee; victoria] ;; 

form_friend_group (user_list :> user_type list) ;; 


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

  method evaluate (x : float) : float = 
     List.fold_right (fun coeff acc -> x *. acc +. coeff) coefficients 0.0

  method solve (c : float) : float list option = None 
 end ;; 

type linear_coefficients = 
 | Constant of float 
 | Linear of float * float (* first element of constructor should be intercept, second should be slope *)

let rec extract_linear_coeffs (coeffs : linear_coefficients) = 
  match coeffs with 
  | Constant c -> [c]
  | Linear (intercept, slope) -> if slope = 0. then extract_linear_coeffs (Constant intercept) else [intercept; slope]  

class linear_polynomial (coefficients : linear_coefficients) : polynomial_type = 
 object (this)

 inherit polynomial (extract_linear_coeffs coefficients) 

 method! solve (c : float) : float list option = 
    let coeffs = this#get_coefficients in 
     match coeffs with 
     | [constant] -> if c = constant then Some [Float.infinity] else None 
     | [intercept; slope] -> Some [(c -. intercept) /. slope]
     | _ -> failwith "solve: Invalid linear polynomial"
  
 end ;; 

 type quadratic_coeffs = 
  | Constant of float 
  | Linear of float * float (* first element is intercept, second is slope *)
  | Quadratic of float * float * float (* Quadratic (c, b, a) corresponds to ax^2 + bx + c *)

let rec extract_quad_coeffs (coeffs: quadratic_coeffs) = 
   match coeffs with 
  | Constant c -> [c]
  | Linear (intercept, slope) -> if slope = 0. then extract_quad_coeffs (Constant intercept) else [intercept; slope]  
  | Quadratic (c, b, a) -> if a = 0. then extract_quad_coeffs (Linear (b, c)) else [c; b; a] ;; 
  

class quadratic_polynomial (coefficients: quadratic_coeffs) : polynomial = 
 object (this)
 inherit polynomial (extract_quad_coeffs coefficients)

 method !solve (c : float) : float list option = 
    let coeffs = this#get_coefficients in 
     match coeffs with 
     | [constant] -> if c = constant then Some [Float.infinity] else None 
     | [intercept; slope] -> Some [(c -. intercept) /. slope]
     | [c_prime; b; a] -> 
        let inner = b ** 2. -. 4. *. a *. (c_prime -. c) in 
        if 
          inner < 0. 
        then None 
        else 
          let root = sqrt inner in 
          let left = (-.b -. root) /. (2. *. a) in 
          let right = (-.b +. root) /. (2. *. a) in 
          if left = right then Some [left] 
          else Some [left; right] 
     | _ -> failwith "solve: Invalid quadratic polynomial"
 end ;; 
