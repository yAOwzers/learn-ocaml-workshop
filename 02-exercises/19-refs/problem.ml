open! Base

(* It is sometimes useful to create a single mutable value. We can do this using
   a [ref]. We can create an [int ref] containing 0 as follows. *)
let x = ref 0

(* Then we can access the value in the ref using the [!] operator, and we can
   update it using the [:=] operator. So, we could increment our ref as
   follows. *)
let () =
  x := !x + 1

(* Write a function min_and_max which returns a tuple containing the minimum and
   maximum values in a non-empty list of positive integers. Your function should
   raise if the list is empty.

   You could do this using [List.fold], but for the purpose of this exercise,
   let's iterate over the list and explicitly maintain refs of the minimum and
   maximum values seen so far instead. *)
let min_and_max lst =
  match lst with 
  | [] -> raise (Invalid_argument "empty list")
  | h :: t -> 
      let min_val = ref h in
      let max_val = ref h in
      List.iter ~f:(fun x -> 
        if x < !min_val then min_val := x;
        if x > !max_val then max_val := x;
      ) t;
      (!min_val, !max_val)
;;
(* By the way, can you guess how a [ref] is implemented under the hood? 

   (Hint: exercise 18.) *)

let%test "Testing min_and_max..." =
  [%compare.equal: int * int] (min_and_max [5;9;2;4;3]) (2,9) 
;;

let%test "Testing min_and_max..." =
  [%compare.equal: int*int] (min_and_max [11;15;7;34]) (7,34)
;;
