
(* ********************************************************************************** *)
(* Task 1: Write a function member *)
(* ********************************************************************************** *)

(* 
   Write a function `member`, which takes a comparison function `c`, a term `t`, 
   and a list `l` and returns `true` if `l` contains an element `e` such that 
   `e` and `t` are equal with respect to `c`.
*)

let rec member cmp elem lst =
  match lst with
  | [] -> false
  | head :: tail -> 
      if cmp elem head = 0 then true
      else member cmp elem tail

(* Testing Task 1 *)
let testing_member () =
   let l =
     [
       __LINE_OF__ ((member compare 3 [1; 2; 3]) = true);
       __LINE_OF__ ((member compare 4 [1; 2; 3]) = false);
       __LINE_OF__ ((member compare 'a' ['a'; 'b'; 'c']) = true);
       __LINE_OF__ ((member compare ('a',5) [(1,2); (3,4); (5,6)]) = false);
       __LINE_OF__ ((member compare ('a',6) [(1,2); (3,4); (5,6)]) = true);
       __LINE_OF__ ((member compare 4 [1; 2; 3]) = false);
     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then Printf.printf "The member test succeeds.\n" else Printf.printf "The member test fails.\n"

(* ********************************************************************************** *)
(* Task 2: Write a function count_occurrences *)
(* ********************************************************************************** *)

let rec update_or_add key freq_list =
  match freq_list with
  | [] -> [(key, 1)]
  | (k, v) :: rest ->
      if k = key then (k, v + 1) :: rest
      else (k, v) :: update_or_add key rest

let count_occurrences lst =
  let rec aux freq = function
    | [] -> freq
    | hd :: tl -> aux (update_or_add hd freq) tl
  in
  let compare_desc (_, n1) (_, n2) = compare n2 n1 in
  aux [] lst |> List.sort compare_desc

(* Testing Task 2 *)
let testing_count_occurrences () =
   let l =
     [
       __LINE_OF__ ((count_occurrences ['a'; 'b'; 'a'; 'c'; 'c'; 'a'; 'd']) = [('a', 3); ('c', 2); ('b', 1); ('d', 1)]);
       __LINE_OF__ ((count_occurrences [0; 0; 0; -2; 3; -1; -1; 3; 3; 0]) = [(0, 4); (3, 3); (-1, 2); (-2, 1)]);
     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then Printf.printf "The count_occurrences test succeeds.\n" else Printf.printf "The count_occurrences test fails.\n"

(* ********************************************************************************** *)
(* Task 3: Write a function drop_last *)
(* ********************************************************************************** *)

let rec drop_last lst =
  match lst with
  | [] -> failwith "Empty list has no last element"
  | [_] -> []
  | head :: next :: rest -> head :: drop_last (next :: rest)

(* Testing Task 3 *)
let testing_drop_last () =
   let l =
     [
       __LINE_OF__ ((drop_last [1; 2; 3; 4]) = [1; 2; 3]);
       __LINE_OF__ ((drop_last [1]) = []);
       __LINE_OF__ ((try Some (drop_last []) with Failure _ -> None) = None);
     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then Printf.printf "The drop_last test succeeds.\n" else Printf.printf "The drop_last test fails.\n"

(* ********************************************************************************** *)
(* Task 4: Write a function drop_last_opt *)
(* ********************************************************************************** *)

let drop_last_opt lst =
  match lst with
  | [] -> None
  | _ -> Some (drop_last lst)

(* Testing Task 4 *)
let testing_drop_last_opt () =
   let l =
     [
       __LINE_OF__ ((drop_last_opt []) = None);
       __LINE_OF__ ((drop_last_opt [1]) = Some []);
       __LINE_OF__ ((drop_last_opt [1;2;3]) = Some [1;2]);
     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then Printf.printf "The drop_last_opt test succeeds.\n" else Printf.printf "The drop_last_opt test fails.\n"

(* ********************************************************************************** *)
(* Task 5: Write a function zip_with *)
(* ********************************************************************************** *)

let rec zip_with func lst1 lst2 =
  match (lst1, lst2) with
  | (x :: xs, y :: ys) -> func x y :: zip_with func xs ys
  | _ -> []

(* Testing Task 5 *)
let testing_zip_with () =
   let l =
     [
       __LINE_OF__ ((zip_with (fun x y -> [x;y]) [1;2;3] [5;6]) = [[1; 5]; [2; 6]]);
       __LINE_OF__ ((zip_with (fun x y -> (x, y)) [1;2;3] ['a';'b']) = [(1, 'a'); (2, 'b')]);
     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then Printf.printf "The zip_with test succeeds.\n" else Printf.printf "The zip_with test fails.\n"

(* ********************************************************************************** *)
(* Task 6: Write a function unzip *)
(* ********************************************************************************** *)

let unzip lst =
  List.fold_right (fun (a, b) (acc_a, acc_b) -> (a :: acc_a, b :: acc_b)) lst ([], [])

(* Testing Task 6 *)
let testing_unzip () =
   let l =
     [
       __LINE_OF__ ((unzip [('a', 1); ('b', 2)]) = (['a'; 'b'], [1; 2]));
       __LINE_OF__ ((unzip []) = ([], []));
     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then Printf.printf "The unzip test succeeds.\n" else Printf.printf "The unzip test fails.\n"

(* ********************************************************************************** *)
(* Task 7: Write a function table_and_scorers *)
(* ********************************************************************************** *)

(* Implementing a simplified version of table_and_scorers *)
let table_and_scorers _ =
  ([], []) (* Placeholder implementation *)

(* ********************************************************************************** *)

(* Run all tests *)
let () =
  testing_member ();
  testing_count_occurrences ();
  testing_drop_last ();
  testing_drop_last_opt ();
  testing_zip_with ();
  testing_unzip ()
