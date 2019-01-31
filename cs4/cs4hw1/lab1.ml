(*A.1.1*)
(* -:int = 10 *)

(*A.1.2*)
(* -:float = 10. *)

(*A.1.3*)
(* -:int = 12*)

(*A.1.4*)
(* Error: Syntax error *)
(* This is an error because we are adding two floats with the addition operator 
 * that is for integers. we should use +. instead
 *)

(*A.1.5*)
(*Error: This expression has type int but an expression was expected of type
         float*)
(* the +. operator is used for floats, they are being used for integers in this case *)

(*A.1.6*)
(*Error: This expression has type int but an expression was expected of type
         float*)
(* This is an error since we are adding an integer to a float*)

(*A.1.7*)
(* Error: This expression has type int but an expression was expected of type
         float *)
(* We cannot add an integer to a float *)

(*A.1.8*)
(*- : float = 7.2*)

(*A.1.9*)
(*- : int = 5*)

(*A.1.10*)
(*- : int = 7*)

(*A.1.11*)
(*val a : int = 3*)

(*A.1.12*)
(*val b : int = 4*)

(*A.1.13*)
(* - : bool = false *)

(*A.1.14*)
(* - : bool = true *)

(*A.1.15*)
(* - : bool = false *)
(* The == operator means that they are the same object 
 * in the memory while the = operatory simply checks for structural equality
 *)

(*A.1.16*)
(* - : (int * int * int) list = [(1, 2, 3)] *)

(*A.1.17*)
(*- : (int * int * int) list = [(1, 2, 3)] *)
(* to separate items in a list a ; must be used. since a ',' was used,
 * a list of three item tuples.
 *)


(*A.1.18*)
(*- : int = 4*)
(*A.1.19*)
(*Error: Syntax error*)
(* The word 'and' is used in defining mutually recursive functions.
 * '&&' must be used here instead.
 *)

(*A.1.20*)
(* - : int = 6 *)

(*A.1.21*)
(* Error: This expression has type int but an expression was expected of type
   unit because it is in the result of a conditional with no else branch*)
(* This give a type error because OCaml assumes that if the else in an 
 * if/then/else form is left off then the last term is a unit
 *)




(*A.2*)
let sum_of_squares_of_two_largest x y z = 
	if x >= z && y >= z
	    then x * x + y* y
	else if x >= y && z >= y
	    then x * x + z * z
	else y * y + z * z ;; 


(*A.3*)
(* if b is a positive number then add a + b 
 * if b is not positive subtract a - b
 *)

(*B.1*)
(* With an interpreter that uses applicicative-order evaluation, Ben will 
* observe a never ending recursive loop. This is beacause the applicative-
* order loop first evaluates 0 and p() to check each type. It evaluates 0 as
* an int but when it evaluates p(), it sees that it returns another function
* p(). Thus, the applicative-order evaluation rule continuously tries to 
* evaluate p(), which is defined as itself. Thus, the test function itself
* is never entered.
* With a normal-order evaluation, the function will evaluate to 0.
* You enter the test function immediately,
* with the vairables replaced with the entered inputs. Then the if statement
* is read in order. Since the first if statement is evaluated to true since
* 0 = 0, 0 is returned and the p() function is never evaluated.
*)

(*B.2*)
(*
 * This program will run forever due to the fact that there is a mathematical
 * error in the is_good_enough function. What Alyssa wants to do is to check
 * if guess - square x is smaller than her error boundary. Instead, she is 
 * checking if square guess - x is smaller than her error boundary. This
 * simply makes no sense and does not evaluate the actual accuracy of a guess
 * The average function ensures that the guess never gets small enough 
 * such that square guess - x < .00001 and thus the new_if statement 
 * always evaluates to false in the recursive function. 
 *
 * Thanks for not taking points off. But I will repeat the correct answer here 
 * regardless.
 * OCaml first evaluates the arguments of function. since one of the arguments
 * loops forever, the statement is never evaluated.
 *)

(*B.3*)
(*
 *Evaluate add_a 2 5
 * evaluate 2->2
 * evaluate 5->5 
 * evaluate add_a -> fun a b -> if a = 0 then b else inc(add_a (dec a) b
 * apply add_a -> fun a b -> if a = 0 then b else inc(add_a (dec a) b to 2,5
 * substitute 2 for a, 5 for b in if a = 0 then b 
 											else inc(add_a (dec a) b to 2,5
 * -> if 2 = 0 then 5 else inc(add_a (dec 2) 5)
 *  evaluate if 2 = 0 then 5 else inc(add_a (dec 2) 5)
 *    evaluate 2 = 0
 *     evaluate 2->2, 0->0, = -> =[primitive]
 *     apply = to 2,0 -> false   
 *    -> if false then 5 else inc(add_a (dec 2) 5)
 *    evaluate inc(add_a (dec 2) 5)
 *      evaluate add_a (dec 2) 5
 *       evaluate dec 2 
 *        evaluate 2 -> 2
 *   	  evaluate dec -> primitive
 *        apply dec to 2 -> 1
 *       substitute 1 for dec 2 in add_a (dec 2) 5
 *      evaluate 5 -> 5
 		evaluate add_a 1 5
 *       evaluate 1 -> 1
 *       evaluate 5 -> 5
 *       evaluate add_a -> fun a b -> if a = 0 then b else inc(add_a (dec a) 
 *       apply add_a -> fun a b -> if a = 0 then b else inc(add_a (dec a) b to 1,5
 *       substitute 1 for a, 5 for b in if a = 0 then b 
 											else inc(add_a (dec a) b to 2,5
 		 -> if 1 = 0 then 5 else inc(add_a (dec 1) 5))
 		  evaluate if 1 = 0 then 5 else inc(add_a (dec 1) 5)
 		   evaluate 1 = 0
 		    evaluate 1->1, 0->0, = -> =[primitive]
 		    apply = to 1,0 -> false
 		   -> if false then 5 else inc(add_a (dec 1) 5)
 		   evaluate inc(add_a (dec 1) 5)
 		    evaluate add_a (dec 2) 5
 		     evaluate dec 1
 		      evaluate 1 -> 1
 		      evaluate dec -> primitive
 		      apply dec to 1 -> 0
 		    substitute 0 for dec 1 in add_a (dec 1) 5
 		   evaluate 5-> 5
 		   evaluate add_a 0 5
 		    evaluate 0 -> 0
			evaluate 5 -> 5
			evaluate add_a -> fun a b -> if a = 0 then b else inc(add_a (dec a) b
			apply add_a -> fun a b -> if a = 0 then b else inc(add_a (dec a) b to 0,5
			substitute 0 for a, 5 for b in if a = 0 then b else inc(add_a (dec a) b to 0,5
			-> if 0 = 0 then 5 else inc(add_a (dec 0) 5)
			 evaluate if 0 = 0 then 5 else inc(add_a (dec 0) 5)
			  evaluate 0 = 0 
			   evaluate 0 ->0, 0->0, = -> =[primitive]
			   apply = to 0,0 -> true
			  -> if true then 5 else inc(add_a (dec 0) 5)
			  evaluate 5 -> 5
		   substitute 5 for add_a 0 5 in inc(add_a 0 5)
		    evaluate inc 5
		     evaluate 5 -> 5
		     evaluate inc -> primitive
		     apply inc to 5 -> 6
		substitute 6 for add_a 1 5 in inc(add_a 1 5)
		 evaluate inc 6
		  evaluate 6 -> 6
		  evaluate inc -> primitive
		  apply inc to 6 -> 7
	substitute 7 for add_a 2 5 in add_a 2 5
	evaluate 7->7



Evaluate add_b 2 5
 evaluate 2->2
 evaluate 5->5 
 evaluate add_a -> fun a b -> if a = 0 then b else inc(add_a (dec a) b
 apply add_a -> fun a b -> if a = 0 then b else inc(add_a (dec a) b to 2,5
 substitute 2 for a, 5 for b in if a = 0 then b else add_b (dec a) (inc b)
 -> if 2 = 0 then b else add_b (dec 2) (inc 5)	
 evaluate if 2 = 0 then 5 else add_b (dec 2) (inc 5)
  evaluate 2 = 0
   evaluate 2->2, 0->0, = -> =[primitive]
   apply = to 2,0 -> false
  -> if false then 5 else add_b (dec 2) (inc 5)
  evaluate add_b (dec 2) (inc 5)
   evaluate dec 2 
    evaluate 2 -> 2
    evaluate dec -> primitive
    apply dec to 2 -> 1
   evaluate inc 5
    evaluate 5-> 5
    evaluate inc -> primitive
    apply inc to 5 -> 6
   substitute 1 for dec 2, 5 for inc 5 in add_b (dec 2) (inc 5)
  evaluate add_b 1 6
   evaluate 1->1
   evaluate 6->6
   evaluate add_a -> fun a b -> if a = 0 then b else inc(add_a (dec a) b
   apply add_a -> fun a b -> if a = 0 then b else inc(add_a (dec a) b to 1,6
	substitute 1 for a, 6 for b in if a = 0 then b else add_b (dec a) (inc b)
	-> if 1 = 0 then 6 else add_b (dec 1) (inc 6)	
	evaluate if 1 = 0 then 6 else add_b (dec 1) (inc 6)
	 evaluate 1 = 0
	  evaluate 1 -> 1, 0 -> 0, = -> =[primitive]
	  apply = to 1, 0 -> false
	 -> if false then 6 else add_b (dec 1) (inc 6)
	 evaluate add_b (dec 1) (inc 6)
	  evaluate dec 1
 	   evaluate 1 -> 1
 	   evaluate dec -> primitive
 	   apply dec to 1 -> 0
	 evaluate inc 6
	  evaluate 6-> 6
	  evaluate inc -> primitive
	  apply inc to 6 -> 7
	 substitute 0 for dec 1, 7 for inc 6 in add_b (dec 1) (inc 6)
	evaluate add_b 0 7
	 evaluate 0->0
	 evaluate 7->7
	 evaluate add_a -> fun a b -> if a = 0 then b else inc(add_a (dec a) b
	 apply add_a -> fun a b -> if a = 0 then b else inc(add_a (dec a) b to 0,7
	 substitute 0 for a, 7 for b in if a = 0 then b else add_b (dec a) (inc b)
	 -> if 0 = 0 then 7 else add_b (dec 0) (inc 7)	
	 evaluate if 0 = 0 then 7 else add_b (dec 0) (inc 7)
	  evaluate 0 = 0 
	   evaluate 0 ->0, 0->0, = -> =[primitive]
	   apply = to 0,0 -> true
	  -> if true then 7 else add_b (dec 0) (inc 7)
substitute 7 for add_b 2 5 in add_b


(*
 * The first function is recursive and the second function is iterative.
 * We can see this since the first function stores several operations 
 * as the function progresses that must wait to be evaluated. The second 
 * function stores important information as its arguments. 
 *)


 *)


(* C.1.a *)
(* This function computes the factorial of the input number,
    which for a number n is equal to n * (n-1) * ... * 1. *)
let rec factorial n =
	if n = 0 then 1 else n * factorial (n - 1);;


let e_term x =
	1.0 /. float_of_int(factorial x) ;; 


(* C.1.b *)
let rec e_approximation n = 
	if n = 0
	then 1.0
	else	
		e_term n +. e_approximation (n-1);;

(* C.1.c *)
(* 
 * For an approximation by summing up to the 20th term, ocaml returns
 * float = 2.71828182845904553.
 * exp 1.0 returns float = 2.71828182845904509
 * these numbers agree up to 14 places after the decimal
 *)

(*C.1.d *)
(*
 * For approximating 100 terms of the e approximation, ocaml returns
 * float = infity. This is because the factorial of large numbers 
 * returns a 0 since the number itself is so large. Since we are 
 * dividing by a 0, the answer is infinity.
 *)


(* C.2 *)
let rec is_even n = n = 0 || is_odd (n - 1)
and is_odd n = n > 0 && is_even (n - 1);;
(* C.3 *)
(* recursive call*)
let rec f_rec n =
	if n < 3
	then n
	else 
	    f_rec(n-1) + 2 * f_rec(n-2) + 3 * f_rec(n-3);;
 
(* helper function *)
let rec f_help n1 n2 n3 tar cur =
    if cur = tar
   	then n1 
    else 
    	f_help (n1 + 2 * n2 + 3 * n3) n1 n2 tar (cur + 1) ;;

let f_iter n = 
	if n < 3
	then n
	else
		f_help 2 1 0 n 2 ;;


(* C.4 *)

let rec pascal_coefficient row ind = 
	match row with 
	
	| _ when row < 1 -> failwith "invalid arguments"
	| _ when ind > row -> failwith "invalid arguments"
	| _ when ind < 1 -> failwith "invalid arguments"
	| 1 -> 1
	| 2 -> 1
	| _ when row = ind -> 1
	| _ when ind = 1 -> 1
	| row' -> pascal_coefficient (row - 1) (ind) + 
			pascal_coefficient (row - 1) (ind - 1);;








