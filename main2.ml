
(* N1 - return last element of list *)

let rec last_element list = 
  match list with
  | [] -> None
  | [x] -> None
  | [x; y] -> Some x
  |  x :: xs -> last_element xs


(* N2 - return last two element of list *)

let rec last_two_element list = 
  match list with
  | [] -> []
  | [x] -> []
  | [x ; y] -> [Some x; Some y]
  |  x :: xs -> last_two_element xs


(* N3 - return n-th element of list *)


let rec some_element list n list_length = 
  match list with
  | [] -> None
  | x :: xs -> 
    if list_length - List.length xs = n + 1 then
      Some x
    else
      some_element xs n list_length

(* Wrapper function to calculate the length of the list *)
let some_element_wrapper list n =
  let length = List.length list in
  some_element list n length

(* Example usage *)



(* N4 return length of list *)


let rec list_length list = 
  match list with
  | [] -> 0
  | x :: xs -> 1 + list_length xs




(* N5 reverse the list *)

let rec rev list =
  match list with
  | [] -> []
  | x :: xs -> rev xs @ [x]




(* N6 is palindrome or not *)

let is_palindrome list = 
  if list = rev list 
    then true
else false;;


is_palindrome ["a"; "b"];;


(* N7 Flatten a nested list structure *)


type 'a node =
  | One of 'a 
  | Many of 'a node list

  let list = [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]


  let rec flatten_node list = 
    match list with
    | One x -> [x]
    | Many nodes -> flatten nodes

and flatten list = 
  match list with
  | [] -> []
  | x :: xs -> flatten_node x @ flatten xs





(* N8 Eliminate consecutive duplicates of list elements. *)

  let rec compress list = 
    match list with
    | [] -> []
    | [x] -> [x]
    | x :: (y :: _ as rest) -> if x = y
      then compress rest
  else x  :: compress rest



  (* N9 Pack consecutive duplicates of list elements into sublists. *)

  let pack list =
    let rec aux current acc rest =
      (* current is used to accumulate elements of the same value consecutively *)
      (* acc is used to accumulate sublists of consecutive elements *)
      (* rest is used to accumulate remaining part of list *)
      match rest with
      | [] -> []
      | [x] -> (x :: current) :: acc
      | x :: (y :: _ as rest) ->
          (* x and y are the first two elements at this moment *)
          (* rest is the remaining part of the list *)
          if x = y then
            aux (x :: current) acc rest
          else
            aux [] ((x :: current) :: acc) rest
    in
    List.rev (aux [] [] list);;
  
  
  

pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;
