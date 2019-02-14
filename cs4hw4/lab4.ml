(* A.1 *)
type mobile = Mobile of branch * branch  (* left and right branches *)
  and branch = 
    | Weight    of int * int     (* length and weight *)
    | Structure of int * mobile  (* length and sub-mobile *)

let make_mobile l r = Mobile (l, r)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

(* A.1.a *)
let left_branch (Mobile(x, _)) = x

let right_branch (Mobile (_, x)) = x

let branch_length = function 
    | Weight (l, w) -> l
    | Structure (l, m) -> l

let branch_structure = function
    | Weight (l, w) -> `Weight w
    | Structure (l, m) -> `Structure m


(* A.1.b *)
let rec branch_weight1 = function
    | Weight (l, w) -> w
    | Structure (l, m) -> (totalweight1 m)
and total_weight1 a = 
    match a with 
    | Mobile (b1, b2) -> (branch_weight1 b1) + (branch_weight1 b2)

    
let rec branch_weight2 b = 
    match (branch_structure b) with 
    | `Weight w -> w 
    | `Structure s -> totalweight2 s
and total_weight2 m = 
    (branch_weight2 (right_branch m)) + (branch_weight2 (left_branch m))


(* A.1.c *)
(* is_balanced returns true if left length * left weigth = right length * right weight *)
let rec is_balanced mob = 
    let help br = (branch_length br) * (branch_weight1 br) in
    match (branch_structure (left_branch mob), branch_structure (right_branch mob)) with 
    | (`Weight _, `Weight _) -> help (left_branch mob) = help (right_branch mob)
    | (`Weight _, `Structure s) -> (help (left_branch mob) = help (right_branch mob)) && is_balanced s
    | (`Structure s, `Weight _) -> (help(left_branch mob) = help (right_branch mob )) && is_balanced s 
    | (`Structure s1, `Structure s2)-> (is_balanced s1) && (is_balanced s2) && (help(left_branch mob) = help (right_branch mob))



(* A.1.d *)
type mobile'  = {left: branch'; right: branch'}
and  branch'  = Branch' of int * contents
and  contents = Weight' of int | Structure' of mobile'

let make_mobile' l r = {left = l; right = r}

let make_weight' l w = Branch' (l, Weight' w)

let make_structure' l m = Branch' (l, Structure' m)

let left_branch' {left = a} = a

let right_branch' {right = b} = b

let branch_length' (Branch' (l,  w)) = l

let branch_structure' (Branch' (l,  b)) = 
    match b with 
    | Weight' w -> `Weight w
    | Structure' s -> `Structure s


    
let rec branch_weight' b = 
    match (branch_structure' b) with 
    | `Weight w -> w 
    | `Structure s -> totalweight' s
and totalweight' m = 
    (branch_weight' (right_branch' m)) + (branch_weight' (left_branch' m))



let rec is_balanced' mob = 
    let help br = (branch_length' br) * (branch_weight' br) in
    match (branch_structure' (left_branch' mob), branch_structure' (right_branch' mob)) with 
    | (`Weight _, `Weight _) -> help (left_branch' mob) = help (right_branch' mob)
    | (`Weight _, `Structure s) -> (help (left_branch' mob) = help (right_branch' mob)) && is_balanced' s
    | (`Structure s, `Weight _) -> (help(left_branch' mob) = help (right_branch' mob )) && is_balanced' s 
    | (`Structure s1, `Structure s2)-> (is_balanced' s1) && (is_balanced' s2) && (help(left_branch' mob) = help (right_branch' mob))


(* A.2 *)
type tree = Tree of elem list
  and elem = 
    | Num of int
    | Sub of tree

let rec square_tree tre = 
    let rec iter = function 
        | [] -> []
        | (Sub a) :: t -> Sub (square_tree a) :: iter t
        | (Num a) :: t -> Num (a * a) :: iter t in
    match tre with 
    | Tree t -> Tree (iter t)


let rec square_tree' (Tree tre) =
    let iter lst = 
        match lst with 
        | (Sub s) -> Sub(square_tree' s)
        | (Num n) -> Num (n * n) in
    Tree (List.map iter tre)



(* A.3 *)
let rec tree_map f tre = 
    let rec iter = function 
        | [] -> []
        | (Sub a) :: t -> Sub (tree_map f a) :: iter t
        | (Num a) :: t -> Num (f a) :: iter t in
    match tre with 
    | Tree t -> Tree (iter t)


(* A.4 *)
let rec subsets = function
    | [] -> [[]]
    | h :: t -> let rest = subsets t in
        rest @ (List.map (fun z -> h :: z ) rest)

(* This function splits the inputted list into its head and the 
 * tail. we then recursively call subsets t on the tail. More specifically 
 * subsets takes rest and 
 *)
(* A.5 *)
let rec accumulate op initial sequence = 
    match sequence with
      | [] -> initial
      | h :: t -> op h (accumulate op initial t)

let map p sequence = 
    accumulate (fun x r -> (p x) :: r) [] sequence

let append seq1 seq2 = 
    accumulate (fun x r -> x :: r) seq2 seq1

let length sequence = 
    accumulate (fun x r-> r + 1) 0 sequence

(* A. 6 *)

let rec accumulate_n op init seqs =
    match seqs with
      | [] -> failwith "empty list"
      | [] :: _ -> []   (* assume all sequences are empty *)
      | h :: t -> accumulate op init (map List.hd seqs) :: accumulate_n op init (map List.tl seqs)

(* A.7 *)

let rec map2 f x y =
    match (x, y) with
      | ([], []) -> []
      | ([], _) -> failwith "unequal lists"
      | (_, []) -> failwith "unequal lists"
      | (_, _) -> (f (List.hd x) (List.hd y)) :: (map2 f (List.tl x) (List.tl y))
  
let dot_product v w = accumulate (+) 0 (map2 ( * ) v w)

let matrix_times_vector m v = map (fun inp -> dot_product inp v) m

let transpose mat = accumulate_n (fun a b -> a :: b) [] mat

let matrix_times_matrix m n =
    let cols = transpose n in
        map (fun x -> matrix_times_vector cols x) m

(* B.1 *)
 let rec filter predicate sequence =
    match sequence with
      | [] -> []
      | h :: t when predicate h -> h :: filter predicate t
      | _ :: t -> filter predicate t

let rec quicksort lst op = 
    match lst with 
    | []-> []
    | h::t -> (quicksort (filter (fun x -> (op) x h) t) op) @ [h] @ (quicksort (filter (fun x -> (not(op x h))) t) op)

(* B.2 *)
(*
 * Quicksort is a generative resursive function since it has to generate two 
 * separate lists and sort those lists seperately before creating a new list that combines 
 * both of them. In short, it performes generative recursion since it generates new lists
 * in its recursive process.
 *)

(* B.3 *)
(*
 * Ben's version of merge sort results in a stack overflow since the function 
 * will enter an infinite recursive loop. When there is a single value in a list that 

 * his passed into his function, it will be split up by the even_half function and then 
 * sorted with itself. Thus, the single element list will be passed in to the function again
 * and will infinitely go through that list.
 *)

(* B.4 *)
let rec insert_in_order new_result a_list cmp = 
    match a_list with 
      | [] -> [new_result]  
      | h :: t when cmp new_result h -> new_result :: a_list
      | h :: t ->  h :: insert_in_order new_result t cmp
  
  let rec insertion_sort a_list cmp =
    match a_list with
      | [] -> []
      | h :: t -> insert_in_order h (insertion_sort t cmp) cmp

(*
 * This is structural recursion since we are decomposing the structure
 * of the list into its natural parts head and tail. 
 *)


(* C.1 *)
type expr =
    | Int of int           (* constant *)
    | Var of string        (* variable *)
    | Add of expr * expr   (* expr1 + expr2 *)
    | Mul of expr * expr   (* expr1 * expr2 *)
    | Pow of expr * int    (* expr^n *)

let pow x y = int_of_float ((float_of_int x) ** (float_of_int y))

let rec simplify1 = function
    | Int x -> Int x
    | Var y -> Var y
    | Add (Int a, Int b) -> Int (a + b)
    | Mul (Int a, Int b) -> Int (a * b)
    | Pow (Int a, b) -> Int (pow a b)
    | Mul (Int 0, _) -> Int 0
    | Mul (_, Int 0) -> Int 0
    | Pow (_, 0) -> Int 1
    | Pow (ex, 1) -> simplify1 ex
    | Mul (Int 1, ex) -> simplify1 ex
    | Mul (ex, Int 1) -> simplify1 ex
    | Add (Int 0, ex) -> simplify1 ex
    | Add (ex, Int 0) -> simplify1 ex
    | Add (ex1, ex2) -> Add (simplify1 ex1, simplify1 ex2)
    | Mul (ex1, ex2) -> Mul (simplify1 ex1, simplify1 ex2)
    | Pow (ex1, x) -> Pow (simplify1 ex1, x)

let rec simplify expr =
    let e = simplify1 expr in
      if expr = e
        then expr
        else simplify e
(* C. 2 *)
let rec deriv x dx =
    match x, dx with 
    | (Int a), _ -> Int 0
    | _ when (Var dx) = x -> Int 1
    | (Var a), b -> Int 0
    | (Add (a, b)), c -> Add ((deriv a c), (deriv b c))
    | (Mul (a , b)), c -> Add (Mul((deriv a c), b), Mul((deriv b c), a))
    | Pow(ex, a), c -> Mul (Mul(Int a, Pow(ex, (a - 1))), deriv ex c)

let derivative expr var =
    let e = simplify expr in
    let d = deriv e var in
      simplify d





