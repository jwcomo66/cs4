open num

(* A.1 *)
(*
 * The space complexity for the fib function is O(N). This is different from 
 * time complexity since time complexity takes into account every single
 * operation while space complexity is only concered with the number
 * of pending operations/ stored variables on the stack. For fib 7, the 
 * largest number of pending operations is 7 since it will traverse the 
 * tree down until it evaluates fib 1. 
 *)

(* A.2.a *)
(*
 * p is applied 5 times when sine 12.15 is evaluated
 *)

(* A.2.b *)
(*
 * the order of growth in space is O(log(a))and number of steps 
 * as a function of a (time comlexity) is also O(log(a))
 *)

(* A.3.1 *)
let rec fast_expt b n =
    let is_even m = m mod 2 = 0 in
    let square m = m * m in
	match n with
    | 0 -> 1
    | _ when is_even n -> square (fast_expt b (n/2))
    | _ -> b * fast_expt b (n-1) 

(* A.3.2 *)

let rec ifast_expt b n = 
    let is_even m = m mod 2 = 0 in
    let square m = m * m in
    let rec counter c b n = 
        match n with 
        | 0 -> c
        | _ when is_even n -> counter c (square b) (n/2)
        | _ -> counter (c * b) b (n - 1) in
    counter 1 b n

(* A.4 *)
let rec fast_mult b n = 
    let double a = a + a in
    let halve a = a/2 in 
    let is_even m = m mod 2 = 0 in

    match n with 
    | 0 -> 0
    | _ when is_even n -> double (fast_mult b (halve n))
    | _ -> b + fast_mult b (n-1)

(* A.5 *)
let rec ifast_mult b n = 
    let double a = a + a in
    let halve a = a/2 in 
    let is_even m = m mod 2 = 0 in
    let rec counter c b n = 
        match n with 
        | 0 -> c
        | _ when is_even n -> counter c (double b) (halve n)
        | _ -> counter (c + b) b (n - 1) in 
    counter 0 b n

(* A.6 *)
(*
 * The worst case space complexity is O(Log(n)) since an input n will be 
 * halved until it is of value 1. Every time it is halved, an operation is 
 * stored on the stack. And complexity will simplify log_2(n) to log(n)
 * The worst case time complexity is O(N) since the number of operations 
 * is equivalent to 2^(log_2(n)). This is equivalent to n.
 *)

(* A.7.1 *)
(*
 * This function represents a linear recursive process. We know this since 
 * the recursive calls continuosly add pending operations to the stack until
 * the base case is reached and the operations can be executed. 
 *))

(* A.7.2 *)
(*
 * The space complexity of this function with respect to its arument n is O(n).
 * We see this since n is decremented until the base case is reach and thus. 
 * The time complexity is also O(n) since the recursive call is linear and only 
 * calls itself once every interation.   
 *)
(*
(* B.1.a *)
(fun x y ->x * (2 + y)) 20 (2 * 4);;

(* B.1.b *)
(fun a b c -> sqrt (b *. b -. 4.0 *. a *. c)) 1.0 20.0 3.0;;

(* B.1.c *)
(*(fun x -> let y = 2 in let z = 3 in x * y * z) 1
-> (fun x -> ((fun y -> let z = 3 in x * y * z ) 2) 1 *)
(fun x->(fun y -> (fun z -> x * y * z)3)2)1;;

(* B.1.d *)
(fun x -> x * x * x)3;;

*)
(* B.2 *)
(*
let x = 2 * 10
and y = 3 + 4
in 
let y = 14 in
  let z = 22 in
    x * y * z

************
evaluate (fun x y -> (fun y -> (fun z-> x * y * z ) 22) 14) (2 * 10) (3 + 4)
evaluate (fun x -> (fun y -> (fun z-> x * y * z ) 22) 14) (2 * 10)
 Evaluate (fun z -> x * y * z) 22
  evaluate 22 -> 22
  evaluate fun z -> x * y * z
  apply fun z -> x * y * z to 22
   substitute 22 for z in x * y * z
   evaluate x * y * 22
 substitute x * y * 22 for (fun z -> x * y * z )22 in (fun y -> (fun z-> x * y * z ) 22) 14)
 evaluate (fun y -> x * y * z ) 14
  evaluate 14 -> 14 
  evaluate fun y -> x * y * 22
  apply fun z -> x * y * 22 to 14
   substitute 14 for y in x * y * 22
   evaluate x * 14 * 22
 substitute x * 14 * 22 for (fun y -> (fun z-> x * y * z ) 22) 14) in (fun x -> (fun y -> (fun z-> x * y * z ) 22) 14) (2 * 10)
  evaluate fun x -> (fun x -> x * 14 * 22) (2 * 10)
   evaluate 2 * 10 -> 20
    apply fun x -> (fun x -> x * 14 * 22) to 20
    substitute 20 for x in x * 14 * 22
    evaluate 20 * 14 * 22 = 6160

************

(* B.3 *)
(fun x y z -> x + y + z) 10 (x * 2) (y + 3);;
(*
 * This function does not work because the multiple x's and y's are not being
 * interpreted as the same value by the cpu. Since we are de claring these 
 * values in the same statement. The fix is quite simple:
 *)
let x = 10 in 
let y = x * 2 in
let z = y + 3 in 
x + y + z ;;

*)

(* C.1 *)
let ni = num_of_int (* convert in-> num *)

let isum term a next b = 
    let rec iter a result = 
        if a >/ b
            then result
            else iter (next a) (result +/ term a)
    in 
    iter a (ni 0) ;;


(* C.2.a *)

(* this function is a recursive process *)
let rec product_rec term a next b =
    if a >/ b
        then (ni 1)
        else term a */ (product_rec term (next a) next b)

let rec factorial_rec n = 
    let term a = a in 
    let next b = b +/ (ni 1) in
    if n =/ (ni 0)
        then (ni 1)
        else product_rec term (ni 1) next n


let pi_product n = 
    let term a = ((a -/ (ni 1)) */ (a +/ (ni 1)) // (a */ a) in
    let next a = a +/ (ni 2) in 
    (ni 4) */ (product_rec term (ni 3) next n)

let pi_approx = float_of_num(pi_product (ni 1000))


(* C.2.b *)
let product_iter term a next b =
    let rec iter a result = 
        if a >/ b
        then result 
        else iter (next a) (result */ term a)
    in
    iter a (ni 1)

let rec factorial_iter n = 
    let term a = a in 
    let next b = b +/ (ni 1) in
    if n =/ (ni 0)
        then (ni 1)
        else product_iter term (ni 1) next n


(* C.3.a *)
(* This function is recursive *)
let rec accumulate_rec combiner null_value term a next b = 
    if a >/ b 
        then null_value
    else combiner (term a) (accumulate_rec combiner null_value term (next a) next b)


(* C.3.b *)
(* This function is iterative *)
let accumulate_iter combiner null_value term a next b = 
    let rec iter a null_value = 
        if a >/ b 
            then null_value
            else iter (next a) (combiner  null_value (term a)) in
    iter a null_value

(* CALL PRODUCT OR SUM HERE *)

let sum term a next b = 
    accumulate_iter ( +/ ) (ni 0) term a next b

let product term a next b = 
    accumulate_iter ( */ ) (ni 1) term a next b


(* C.4 *)
let compose f g n =
    f (g n)

(* C.5 *)
let rec repeated f n =
    if n = 0 
    then fun a -> a
    else compose f (repeated f (n - 1))

(* C.6 *)
let smooth dx f n =
    (f (n+ dx)) + (f n) + ((f (n - dx))

let n_smoothed dx f n = 
    (repeated (smooth dx) n) f



(* D.1 *)
let is_prime n = 
    let rec helper c n = 
        match n with 
        | _ when n < 2 -> false
        | 2 -> true
        | _ when float_of_int c > sqrt (float_of_int n) -> true
        | _ when n mod c = 0 -> false
        | _ -> helper (c + 1) n in
    helper 2 n ;;


(* D.2 *)
let smallest_prime_factor n = 
    let rec helper c n =
        match n with
        | _ when is_prime n -> failwith "number is prime"
        | _ when float_of_int c > sqrt (float_of_int n) -> failwith "no prime factors"
        | _ when n mod c = 0 && is_prime c -> c 
        | _ -> helper (c + 1) n in 
    helper 2 n ;;











