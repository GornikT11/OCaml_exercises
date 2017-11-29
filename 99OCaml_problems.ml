
(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. *)

let rec last = function
  | [] -> None
  | [x] -> Some x
  | _::xs -> last xs
  
(* 2. Find the last and penultimate elements of the list. *)
  
let rec last_two = function
  | [] | [_] -> None
  | [x; y] -> Some (x, y)
  | _::xs -> last_two xs
  
(* 3. Find the kth element of a list. *)

let rec at k l = 
  match (k, l) with
  | (_, []) -> None
  | (1, x::_) -> Some x 
  | (k, _::xs) -> at (k-1) xs
  
(* 4. Find the number of elements of a list. *)

let length l =
  let rec len acc = function
    | [] -> acc
	| _::xs -> len (acc+1) xs
	in len 0 l
	
(* 5. Reverse a list. *)

let rev l =
  let rec reverse acc = function
    | [] -> acc
	| x::xs -> reverse (x::acc) xs
	in reverse [] l
	
(* 6. Find out wether a list is a palindrome. *)

let is_palindrome l = (l = rev l) 

(* 7. Flaten a nested list structure. *)

type 'a node =
  | One of 'a
  | Many of 'a node list


let flatten l =
  let rec flat acc = function
  | [] -> acc
  | One x::xs -> flat (x::acc) xs
  | Many l::ls ->  flat ((flat [] l)@acc) ls
  in rev (flat [] l)

(* 8. Eliminate consecutive duplicates of list elements. *)

let rec compress = function
    | [] -> []
	| [x] -> [x]
	| a::(b::xs as t) -> if a = b then compress t 
	else a :: compress t
	
(* 9. Pack consecutive duplicates of list elements into sublists. *)

let pack l = 
  let rec packing edacc acc = function
      | [] -> []
	  | [x] -> (x::acc)::edacc
	  | a::(b::_ as t) -> 
	    if a = b then packing edacc (a::acc) t 
	    else packing ((a::acc)::edacc) [] t
	  in rev (packing [] [] l)

(* 10. Run-length encoding of a list. *)

let encode l =
  let rec encoding num acc = function
    | [] -> []
	| [x] -> (num, x)::acc
	| a::(b::_ as t) ->
	  if a = b then encoding (num + 1) acc t
	  else encoding 1 ((num, a)::acc) t
	in rev (encoding 1 [] l)

(* 11. Modified run-length encoding. *)

type 'a rle =
    | One of 'a
    | Many of int * 'a

let encode1 l =
  let rec encoding num acc = function
    | [] -> []
	| [x] -> if num = 1 then (One x)::acc else (Many (num, x))::acc
	| a::(b::_ as t) ->
	  if a = b then encoding (num + 1) acc t
	  else if num = 1 then  encoding 1 ((One a)::acc) t
	  else encoding 1 ((Many (num, a))::acc) t
	in rev (encoding 1 [] l)

(* 12. Decode a run-length encoded list. *)

let dec l =
 match l with
 | (Many (k,x)) -> (k,x)
 |(One x) -> (1,x)

let rec expand acc = function
  | (0, x) -> acc
  | (k, x) -> expand (x::acc) ((k-1), x)


let rec decode = function
    | [] -> []
	| x::xs -> (expand [] (dec x))@(decode xs)

(* 13. Run-length encoding of a list (direct solution). *)


(* 14. Duplicate the elements of a list. *)

let rec duplicate = function
  | [] -> []
  | x::xs -> [x; x]@duplicate xs

(* 15. Replicate the elements of a list a given number of times. *)

let rec replicate l n =
  let rec repl acc x = function
    | 0 -> acc
	| k -> repl (x::acc) x (k-1)
	in
  match l with
  | [] -> []
  | x::xs -> (repl [] x n)@(replicate xs n)








