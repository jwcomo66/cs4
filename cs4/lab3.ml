
(* A.1 *)
(* define new point type *)
type point = 
    {x : float; y : float}
(* define new segment type *)
type segment = 
    {startp : point; endp : point}



(* define functions *)
let midpoint_segment {startp = a; endp = b} =
    let point {x = a; y = b} {x = c; y = d} =
    {x = (1.0 /. 2.0) *. (a +. c); y = (1.0 /. 2.0) *. (b +. d)}
in
    point a b

let segment_length {startp = a; endp = b}=
    let len {x = a; y = b} {x = c; y = d} =
        sqrt ((a -. c) *. (a -. c) +. (b -. d) *. (b -. d))
    in 
    len a b 
    
let print_point {x = a; y = b}  =  Printf.printf "(%g, %g)" a b


(* Define an abstraction of a point and segment *)
let make_point a b = 
    {x = a; y = b}

let make_segment a b  = 
    {startp = a; endp = b}

let get_coords {x = a; y = b} =
    (a, b)

     
let get_points {startp = a; endp = b} = 
    (a, b)


(* A.2 *)
type rectangle = {lowleft : point; upright: point}

let rectangle_lower_segment {lowleft = a; upright = b} = 
    make_segment a {x = b.x; y = a.y}

let rectangle_upper_segment {lowleft = a; upright = b} = 
    make_segment {x = a.x; y = b.y} b

let rectangle_left_segment {lowleft = a; upright = b} = 
    make_segment a {x = a.x; y = b.y}

let rectangle_right_segment {lowleft = a; upright = b} = 
    make_segment {x = b.x; y = a.y} b

let rectangle_perimeter {lowleft = a; upright = b} =
    2.0 *. (segment_length (rectangle_lower_segment {lowleft = a; upright= b})) +. 
    2.0 *. (segment_length (rectangle_left_segment {lowleft = a; upright= b}))

let rectangle_area {lowleft = a; upright = b} = 
    (segment_length (rectangle_lower_segment {lowleft = a; upright = b})) *. 
    (segment_length (rectangle_left_segment {lowleft = a; upright = b}))
(* making type 2 *)

type rectangle2 = {leftx : float; rightx : float; lowy : float; highy : float}

let rectangle_lower_segment2 {leftx = lx; rightx = rx; lowy = ly; highy = hy} = 
    make_segment (make_point lx ly) (make_point rx ly)

let rectangle_upper_segment2 {leftx = lx; rightx = rx; lowy = ly; highy = hy} = 
    make_segment (make_point lx hy) (make_point rx hy)

let rectangle_left_segment2 {leftx = lx; rightx = rx; lowy = ly; highy = hy} = 
    make_segment (make_point lx ly) (make_point lx hy)

let rectangle_right_segment2 {leftx = lx; rightx = rx; lowy = ly; highy = hy} = 
    make_segment (make_point rx ly) (make_point rx hy)

let rectangle_perimeter2 {leftx = lx; rightx = rx; lowy = ly; highy = hy} = 
    2.0 *. (segment_length (rectangle_lower_segment2 {leftx = lx; rightx = rx; lowy = ly; highy = hy})) +. 
    2.0 *. (segment_length (rectangle_left_segment2 {leftx = lx; rightx = rx; lowy = ly; highy = hy}))

let rectangle_area2 {leftx = lx; rightx = rx; lowy = ly; highy = hy} = 
    (segment_length (rectangle_lower_segment2 {leftx = lx; rightx = rx; lowy = ly; highy = hy})) *. 
    (segment_length (rectangle_left_segment2 {leftx = lx; rightx = rx; lowy = ly; highy = hy}))

let make_rectangle a b = 
    {lowleft = a ; upright = b}

let make_rectangle2 a b c d = 
    {leftx = a; rightx = b; lowy = c; highy = d}

(* A.3 *)

let make_pair x y = fun m -> m x y
(* Or, equivalently: let make_pair x y m = m x y *)
let first z = z (fun x y -> x)
let second z = z (fun x y -> y)


(* 
    first (make_pair x y) -> (make_pair x y) (fun x y -> x)
        (fun m -> m x y) (fun x y -> x)
        (m x y) (fun x y -> x)
        substitute fun x y -> x for m in m x y
        fun x y -> x 
    second (make_pair x y) -> (make_pair x y) (fun x y -> y)
        (fun m -> m x y) (fun x y -> y)
        (m x y) (fun x y -> y)
        substitute fun x y -> y for m in m x y
        fun x y -> y

    FULL SUBSTITUION MODEL FOR second 
evaluate (fun second z -> z (fun x y -> y)) (make_pair 1 2)
 evaluate fun second z -> z (fun x y -> y)
 evaluate (make_pair 1 2 )
  evaluate (fun make_pair x y -> m x y)
   evaluate m -> m
   evaluate 1 -> 1
   evaluate 2 -> 2
 substitute m 1 2 for z in z (fun x y -> y)
 evaluate (m 1 2) (fun x y -> y)
  evaluate (fun x y -> y)
  apply (m 1 2) to (fun x y -> y)
  evaluate fun 1 2
   1 -> 1
   2 -> 2 
  apply fun to 1 2 
  2



*)

(* A.4 *)

let pow a b =
    int_of_float ((float_of_int a) ** (float_of_int b))

let int_log a b =
    let rec helper a b c = 
        match b with 
        | 1 -> c
        | _ when b mod a <> 0 -> c
        | _ -> helper a (b / a) (c + 1) in 
    helper a b 0 


let make_pairi a b =
    (pow 2 a) * (pow 3 b)

let firsti a = 
    int_log 2 a 

let secondi a = 
    int_log 3 a 

(* A.5 *)


let zero = []
  
let is_zero = function
    | [] -> true
    | () :: _ -> false
  

let succ u = () :: u


let prev lst = 
    match lst with
    | [] -> invalid_arg "Cannot enter empty list"
    | h :: t -> t

let rec integer_to_unary n = 
    if n = 0 
    then []
    else () :: integer_to_unary (n - 1) 

let unary_to_integer lst = 
    let rec helper lst b =
        if (integer_to_unary b) = lst 
        then b 
        else helper lst (b + 1)
    in helper lst 0 

let unary_add lst1 lst2 = 
    lst1 @ lst2 



type nat = Zero | Succ of nat

let zero' = Zero

let is_zero' = function
    | Zero -> true
    | Succ _ -> false

let succ' u = Succ u



let prev' lst = 
    match lst with 
    | Zero -> invalid_arg "Cannot enter empty list"
    | Succ a -> a



let rec integer_to_unary' n = 
    if n = 0
    then Zero
    else Succ (integer_to_unary (n - 1))


let unary_to_integer' lst = 
    let rec helper lst b = 
        if (integer_to_unary b) = lst 
        then b 
        else helper lst (b + 1)
    in helper lst 0

    
let unary_add' n1 n2 = 
    integer_to_unary' ((unary_to_integer') n1 + (unary_to_integer' n2))



(* Comment here *)
(* A. 6 *)
let zerof = fun s -> fun z -> z  

let add1 n = fun s z -> s (n s z)

let one = fun s z -> s z 

let two = fun s z -> s (s z)

let three = fun s z -> s (s (s z))

let four = fun s z -> s (s (s (s z)))

let five = fun s z -> s (s (s (s (s z))))

let six = fun s z -> s (s (s (s (s (s z)))))

let seven = fun s z -> s (s (s (s (s (s (s z))))))

let eight = fun s z -> s (s (s (s (s (s (s (s z)))))))

let nine = fun s z -> s (s (s (s (s (s (s (s (s z))))))))

let ten = fun s z -> s (s (s (s (s (s (s (s (s (s z)))))))))


let add m n s z = 
    m s n s z

let church_to_integer a =
    a (fun n -> n + 1) 0 

(* A.7 *)
(*
 * church_to_integer zerof returns an integer since the input to 
 * church_to_integer takes type ((int -> int) -> int -> 'a) as its input
 * since we are entering zerof as the input, we know that zerof has type 
 * ((int -> int) -> int -> 'a). And we know that zerof is 'a -> 'b -> 'b = <fun>.
 * thus we know that 'a must be (int -> int). Therefore, we know that 'b is
 * an int and since we see that 'b -> 'b in zerof, we know that the corresponding 
 * 'a in ((int -> int) -> int -> 'a) but also be an int. 
 * since the church_to_integer function returns 'a, we know it must return an int
 *
 *
 * church_to_integer one returns an integer since church_to_integer is a
 * function of type ((int -> int) -> int -> 'a) -> 'a.
 * one is a function of type ('a -> 'b) -> 'a -> 'b 
 * we can clearly see that ('a -> 'b) in one corresponds to (int -> int) in 
 * the input of church_to_integer. Thus both 'a and 'b are integers. The 'a in 
 * church_to_integer corresponds to 'b in one and since 'b in one is an int, 'a,
 * the output of church_to_integer is also an int.
 *)


(* Part B *)

(* B.1 *)
let rec last_sublist = function 
    | [] -> invalid_arg "last_sublist: empty list"
    | [x] -> x :: []
    | h :: t -> last_sublist t


(* B.2 *)

let reverse lst =
    let rec iter lst rev = 
       match lst with 
       | [] -> rev
       | h :: t ->  iter t (h :: rev) 
       in 
    iter lst []

(* B.3 *)


let rec square_list = function
    | [] -> []
    | h :: t -> (h * h) :: square_list t
  
let square_list2 lst = 
    List.map (fun a -> a * a) lst

(* B.4 *)

(* 
 * Defining squeare list like Louis did originally produces the answer list in
 * reverse order since the a :: b constructor will append a to b in the 
 * beginning of the list. Thus, since this an iterative process, the first
 * value of the list will be squared and added to the list. The second value
 * will be squared and then added to the front of the list with the first value
 * staying at the end of the lsit. We can clearly see why this produces the reverse
 * list.
 *
 * Switching the order of the values around the :: operator will not work since the ::
 * operator must have a list on its right side. Thus, there will be a compiling error if 
 * louis tries to append a list to a number. 
 *)
(* 
let square_listi items =
    let rec iter things answer =
      match things with
        | [] -> answer
        | h :: t -> iter t (answer @ [h * h])
    in iter items []

* This function does not have good space complexity since the @ operator
* creates a new list everytime it is called
*)


let rec count_negative_numbers = function
    | [] -> 0
    | h :: t when h < 0 -> 1 + count_negative_numbers t 
    | h :: t -> count_negative_numbers t 
     
let power_of_two_list n =
    let rec iter n c = 
        match n with 
        | 0 -> 1 :: c
        | _ -> iter (n - 1) ((pow 2 n) :: c)
in
    iter n []

let prefix_sum lst =
    let rec iter lst c s =
        match lst with
        | [] -> c
        | h :: t -> (s + h) :: iter t c (s + h)
    in 
    iter lst [] 0


(* B. 6 *)
let deep_reverse lst =
    let rec iter lst c =
        match lst with 
        | [] -> c
        | h :: t -> iter t ((reverse h) :: c) in
    iter lst []


(* B.7 *)
type 'a nested_list = 
    | Value of 'a
    | List of 'a nested_list list

let deep_reverse_nested lst = 
    let rec iter lst c =
        match lst with 
        | [] -> c
        | Value h :: t -> iter t (Value h :: c)
        | List h :: t -> iter t (List (iter h []) :: c)
        

    in 
    match lst with 
    | Value v -> lst 
    | List lst -> List (iter lst [])

















