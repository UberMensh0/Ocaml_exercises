(* Problem 1:
 *
 * Find functions f1, f2, and f3, such that:
 * - fold_left f1 [] [(a1, b1); ...; (an, bn)] computes the list [(b1, a1); ...; (bn, an)].
 * - fold_left f2 [] [a_0; ...; a_n] computes the list [a_n; a_{n-2}; ...; a_0; ...; a_{n-1}].
 * - fold_left f3 (fun _ -> 0) [(k1, v1); ...; (kn, vn)] computes a function g such that g(ki) = vi.
 *)

let f1 acc (a, b) = acc @ [(b, a)]
let f2 acc x = x :: List.rev acc
let f3 acc (k, v) = fun x -> if x = k then v else acc x

(* Problem 2:
 *
 * Rewrite the following functions in a tail-recursive form:
 * - map_tr: Maps a function f over a list using tail recursion.
 * - replicate_tr: Creates a list with n repetitions of x using tail recursion.
 *)

let map_tr f l =
  let rec aux acc = function
    | [] -> List.rev acc
    | h :: t -> aux (f h :: acc) t
  in aux [] l

let replicate_tr n x =
  let rec aux acc n =
    if n < 1 then List.rev acc
    else aux (x :: acc) (n - 1)
  in aux [] n

(* Problem 3:
 *
 * Implement a mapping function for lazy lists:
 * - map_over_custom_llist for custom lazy lists.
 * - map_over_ocaml_llist for OCaml lazy lists.
 *)

type 'a custom_llist = unit -> 'a custom_cell
and 'a custom_cell = NilC | ConsC of 'a * 'a custom_llist

type 'a ocaml_llist = 'a ocaml_cell Lazy.t
and 'a ocaml_cell = NilO | ConsO of 'a * 'a ocaml_llist

let rec map_over_custom_llist f l =
  match l () with
  | NilC -> fun () -> NilC
  | ConsC (h, t) -> fun () -> ConsC (f h, map_over_custom_llist f t)

let rec map_over_ocaml_llist f l =
  match Lazy.force l with
  | NilO -> lazy NilO
  | ConsO (h, t) -> lazy (ConsO (f h, map_over_ocaml_llist f t))

(* Problem 4:
 *
 * Implement a merging function that combines two sorted lazy lists:
 * - merge_custom_llists for custom lazy lists.
 * - merge_ocaml_llists for OCaml lazy lists.
 *)

let rec merge_custom_llists l1 l2 =
  match (l1 (), l2 ()) with
  | NilC, _ -> l2
  | _, NilC -> l1
  | ConsC (h1, t1), ConsC (h2, t2) ->
      if h1 <= h2
      then fun () -> ConsC (h1, merge_custom_llists t1 l2)
      else fun () -> ConsC (h2, merge_custom_llists l1 t2)

let rec merge_ocaml_llists l1 l2 =
  match Lazy.force l1, Lazy.force l2 with
  | NilO, _ -> l2
  | _, NilO -> l1
  | ConsO (h1, t1), ConsO (h2, t2) ->
      if h1 <= h2
      then lazy (ConsO (h1, merge_ocaml_llists t1 l2))
      else lazy (ConsO (h2, merge_ocaml_llists l1 t2))

(* Problem 5:
 *
 * Implement a function that removes duplicates from sorted lazy lists:
 * - drop_dupl_custom_llist for custom lazy lists.
 * - drop_dupl_ocaml_llist for OCaml lazy lists.
 *)

let rec drop_dupl_custom_llist l =
  match l () with
  | NilC -> fun () -> NilC
  | ConsC (h, t) ->
      match t () with
      | NilC -> fun () -> ConsC (h, fun () -> NilC)
      | ConsC (x, xs) ->
          if x = h then drop_dupl_custom_llist t
          else fun () -> ConsC (h, drop_dupl_custom_llist t)

let rec drop_dupl_ocaml_llist l =
  match Lazy.force l with
  | NilO -> lazy NilO
  | ConsO (h, t) ->
      match Lazy.force t with
      | NilO -> lazy (ConsO (h, lazy NilO))
      | ConsO (x, _) ->
          if x = h then drop_dupl_ocaml_llist t
          else lazy (ConsO (h, drop_dupl_ocaml_llist t))

(* Problem 6:
 *
 * Implement a function that lazily computes Hamming numbers:
 * - hamming_custom_llist for custom lazy lists.
 * - hamming_ocaml_llist for OCaml lazy lists.
 *)

let is_hamming x =
  let rec remove_factors n x =
    if x mod n = 0 then remove_factors n (x / n) else x
  in remove_factors 5 (remove_factors 3 (remove_factors 2 x)) = 1

let hamming_custom_llist =
  let rec from n = fun () -> ConsC (n, from (n + 1)) in
  let rec filter p l =
    fun () ->
      match l () with
      | NilC -> NilC
      | ConsC (h, t) ->
          if p h then ConsC (h, filter p t) else (filter p t) ()
  in filter is_hamming (from 1)

let hamming_ocaml_llist =
  let rec from n = lazy (ConsO (n, from (n + 1))) in
  let rec filter p l =
    lazy (
      match Lazy.force l with
      | NilO -> NilO
      | ConsO (h, t) ->
          if p h then ConsO (h, filter p t) else Lazy.force (filter p t)
    )
  in filter is_hamming (from 1)

(* Tests can be included here if needed *)
