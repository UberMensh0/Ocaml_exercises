(* Task 1: Check if an element is a member of a list using a custom comparator. *)
(* Implement a function `member` that takes a comparator function, an element, 
   and a list, and returns true if the element is in the list. *)

let rec member cmp elem lst =
  match lst with
  | [] -> false
  | head :: tail -> 
      if cmp elem head = 0 then true
      else member cmp elem tail


(* Task 2: Count the occurrences of elements in a list and return a frequency list. *)
(* Implement a function `count_occurrences` that takes a list and returns a list of 
   tuples (element, count) sorted in descending order of counts. *)

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


(* Task 3: Drop the last element of a list. *)
(* Implement a function `drop_last` that removes the last element of a list. *)

let rec drop_last lst =
  match lst with
  | [] -> failwith "Cannot drop from an empty list"
  | [_] -> []
  | head :: next :: rest -> head :: drop_last (next :: rest)


(* Task 4: Drop the last element of a list and return an option type. *)
(* Implement a function `drop_last_opt` that removes the last element of a list 
   and wraps the result in an option type. Return None if the list is empty. *)

let drop_last_opt lst =
  match lst with
  | [] -> None
  | _ -> Some (drop_last lst)


(* Task 5: Zip two lists with a custom function. *)
(* Implement a function `zip_with` that combines two lists element-wise 
   using a provided function. *)

let rec zip_with func lst1 lst2 =
  match (lst1, lst2) with
  | (x :: xs, y :: ys) -> func x y :: zip_with func xs ys
  | _ -> []


(* Task 6: Unzip a list of pairs into two separate lists. *)
(* Implement a function `unzip` that takes a list of pairs and returns 
   a tuple of two lists. *)

let unzip lst =
  List.fold_right (fun (a, b) (acc_a, acc_b) -> (a :: acc_a, b :: acc_b)) lst ([], [])


(* Task 7: Process a dataset of football team statistics. *)
(* Implement a function `table_and_scorers` that takes a list of tuples representing 
   football team data and returns a sorted table and a list of top scorers. *)

let table_and_scorers data =
  let aggregate_records acc (team, games, wins, draws, losses, gf, ga, points, scorers) =
    let update team_data =
      match team_data with
      | [] -> [(team, games, wins, draws, losses, gf, ga, points)]
      | hd :: tl -> 
          if team = fst hd then (team, games + snd hd) :: tl
          else hd :: update tl
    in
    update acc
  in
  let records = List.fold_left aggregate_records [] data in
  let compare_teams (t1, _, _, _, _, _, _, p1) (t2, _, _, _, _, _, _, p2) =
    compare p2 p1
  in
  let sorted_records = List.sort compare_teams records in
  (sorted_records, [])
