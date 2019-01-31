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


(* B.3 *)
(fun x y z -> x + y + z) 10 (x * 2) (y + 3);;
(*
 * This function does not work because the multiple x's and y's are not being
 * interpreted as the same value by the cpu. Since we are declaring these 
 * values in the same statement. The fix is quite simple:
 *)
let x = 10 in 
let y = x * 2 in
let z = y + 3 in 
x + y + z ;;


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











