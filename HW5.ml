(* Assignment 1: More Students! *)

type student = {
  first_name: string;
  last_name: string;
  id: int;
  semester: int;
  grades: (int * float) list;
}

type database = student list
let student_db : database = []

(* Find a student by ID in the database *)
let rec find_student student_id db = 
  match db with
  | [] -> None
  | student :: db' ->
      if student.id = student_id then Some student
      else find_student student_id db'

(* 1.1 Remove a student by ID from the database *)
let rec remove_by_id target_id db =
  match db with
  | [] -> []
  | student :: db' ->
      if student.id = target_id then db'
      else student :: remove_by_id target_id db'

(* 1.2 Count the number of students in a specific semester *)
let rec count_in_semester target_semester db =
  match db with
  | [] -> 0
  | student :: db' ->
      if student.semester = target_semester then
        1 + count_in_semester target_semester db'
      else
        count_in_semester target_semester db'

(* 1.3 Compute the average grade of a student by ID *)
let avg_grade grades =
  let rec avg_accumulator grades sum count =
    match grades with
    | [] -> (sum, count)
    | (_, grade) :: grades' ->
        avg_accumulator grades' (sum +. grade) (count + 1)
  in
  let sum, count = avg_accumulator grades 0. 0 in
  if count = 0 then 0. else sum /. float_of_int count

let student_avg_grade target_id db =
  match find_student target_id db with
  | None -> 0.
  | Some student -> avg_grade student.grades

(* 1.4 Compute the average grade for a specific course *)
let rec find_course_in_student_grades target_course_id grades =
  match grades with
  | [] -> None
  | (course_id, grade) :: grades' ->
      if course_id = target_course_id then Some grade
      else find_course_in_student_grades target_course_id grades'

let rec accumulate_course_grades target_course_id db =
  match db with
  | [] -> []
  | student :: db' ->
      match find_course_in_student_grades target_course_id student.grades with
      | None -> accumulate_course_grades target_course_id db'
      | Some grade -> grade :: accumulate_course_grades target_course_id db'

let course_avg_grade target_course_id db =
  let rec avg_accumulator grades sum count =
    match grades with
    | [] -> (sum, count)
    | grade :: grades' ->
        avg_accumulator grades' (sum +. grade) (count + 1)
  in
  let sum, count = avg_accumulator (accumulate_course_grades target_course_id db) 0. 0 in
  if count = 0 then 0. else sum /. float_of_int count

(* Assignment 2: List Mishmash *)
let rec interleave3 lst1 lst2 lst3 = 
  match (lst1, lst2, lst3) with
  | x1 :: xs1, x2 :: xs2, x3 :: xs3 -> x1 :: x2 :: x3 :: interleave3 xs1 xs2 xs3
  | x1 :: xs1, x2 :: xs2, [] -> x1 :: x2 :: interleave3 xs1 xs2 []
  | x1 :: xs1, [], x3 :: xs3 -> x1 :: x3 :: interleave3 xs1 [] xs3
  | [], x2 :: xs2, x3 :: xs3 -> x2 :: x3 :: interleave3 [] xs2 xs3
  | x1 :: xs1, [], [] -> x1 :: interleave3 xs1 [] []
  | [], x2 :: xs2, [] -> x2 :: interleave3 [] xs2 []
  | [], [], x3 :: xs3 -> x3 :: interleave3 [] [] xs3
  | [], [], [] -> []

(* Assignment 3: OCamlfication *)
let foo (x : int) (y : int) (b : bool) : int =
  let x, y = if x > y then y, x else x, y in
  let rec increment x y b =
    if x < y then
      if b then increment (x + 1) y (not b)
      else increment x (y - 1) (not b)
    else x
  in
  increment x y b

(* Assignment 4: Polynomial Party *)
let eval_poly (x : float) (polynomial : float list) : float =
  let rec eval_accumulator poly power sum =
    match poly with
    | [] -> sum
    | coeff :: poly' -> eval_accumulator poly' (power -. 1.) (sum +. coeff *. (x ** power))
  in
  eval_accumulator polynomial (float_of_int (List.length polynomial - 1)) 0.

let derive_poly (polynomial : float list) : float list =
  let rec derive_accumulator poly power result =
    match poly with
    | [] -> List.rev result
    | _ :: [] -> List.rev result
    | coeff :: poly' -> derive_accumulator poly' (power -. 1.) ((coeff *. power) :: result)
  in
  derive_accumulator polynomial (float_of_int (List.length polynomial - 1)) []

