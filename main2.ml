
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





