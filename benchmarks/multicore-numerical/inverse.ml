type matrix = (int * int) * float array array

exception NonSquare

exception ImproperDimensions

let empty (rows : int) (columns : int) : matrix =
  if rows > 0 && columns > 0 then
    try
      let m = Array.make_matrix rows columns 0. in
      ((rows, columns), m)
    with Invalid_argument _ -> raise ImproperDimensions
  else (* dimension is negative or 0 *)
    raise ImproperDimensions

let get_elt (((n, p), m) : matrix) ((i, j) : int * int) : float =
  if i <= n && j <= p then m.(i - 1).(j - 1) else raise ImproperDimensions

let get_column (((n, p), m) : matrix) (column : int) : int * float array =
  if column <= p then (
    let column' = Array.make n 0. in
    for i = 0 to n - 1 do
      column'.(i) <- m.(i).(column - 1)
    done;
    (n, column') )
  else raise (Failure "Column out of bounds.")

let get_row (((n, p), m) : matrix) (row : int) : int * float array =
  if row <= n then
    let row' = Array.map (fun x -> x) m.(row - 1) in
    (p, row')
  else raise (Failure "Row out of bounds.")

let set_row (((n, p), m) : matrix) (row : int) (a : float array) : unit =
  if row <= n then (
    assert (Array.length a = p);
    for i = 0 to p - 1 do
      m.(row - 1).(i) <- a.(i)
    done )
  else raise (Failure "Row out of bounds.")

let swap_row (m : matrix) (r1 : int) (r2 : int) : unit =
  let len1, row1 = get_row m r1 in
  let len2, row2 = get_row m r2 in
  let _ = assert (len1 = len2) in
  let _ = set_row m r1 row2 in
  let _ = set_row m r2 row1 in
  ()

let scale_row (m : matrix) (num : int) (sc : float) : unit =
  let _len, row = get_row m num in
  let new_row = Array.map (fun x -> sc *. x) row in
  set_row m num new_row

let sub_mult (m : matrix) (r1 : int) (r2 : int) (sc : float) : unit =
  let len1, row1 = get_row m r1 in
  let len2, row2 = get_row m r2 in
  let _ = assert (len1 = len2) in
  for i = 0 to len1 - 1 do
    (* Arrays are 0-indexed *)
    row1.(i) <- row1.(i) -. (sc *. row2.(i))
  done;
  set_row m r1 row1

let set_column (((n, p), m) : matrix) (column : int) (a : float array) : unit =
  if column <= p then (
    assert (Array.length a = n);
    for i = 0 to n - 1 do
      m.(i).(column - 1) <- a.(i)
    done )
  else raise (Failure "Column out of bounds.")

let compare_helper (e1 : float) (e2 : float) (ind1 : int) (ind2 : int) :
    float * int =
  match Stdlib.compare e1 e2 with
  | 0 -> (e2, ind2)
  | v -> if v >= 0 then (e1, ind1) else (e2, ind2)

let find_max_col_index (array1 : float array) (start_index : int) : int option =
  let rec find_index (max_index : int) (curr_max : float) (curr_index : int)
      (arr : float array) =
    if curr_index = Array.length arr then
      if curr_max = 0. then None
      else Some (max_index + 1) (* Arrays are 0-indexed but matrices aren't *)
    else
      match Stdlib.compare arr.(curr_index) 0. with
      | 0 -> find_index max_index curr_max (curr_index + 1) arr
      | v ->
        if v >= 0 then begin
          let el, index =
            compare_helper arr.(curr_index) curr_max curr_index max_index
          in
          find_index index el (curr_index + 1) arr
        end else begin
          let abs_curr_elt = 0. -. arr.(curr_index) in
          let el, index =
            compare_helper abs_curr_elt curr_max curr_index max_index
          in
          find_index index el (curr_index + 1) arr
        end
  in
  find_index 0 0. (start_index - 1) array1

let row_reduce (mat : matrix) : matrix =
  let rec row_reduce_h (n_row : int) (n_col : int) (mat2 : matrix) : unit =
    let (num_row, _num_col), _arr = mat2 in
    if n_col = num_row + 1 then ()
    else
      let _, col = get_column mat2 n_col in
      match find_max_col_index col n_row with
      | None (* Column all 0s *) -> row_reduce_h n_row (n_col + 1) mat2
      | Some index ->
          swap_row mat2 index n_row;
          let pivot = get_elt mat2 (n_row, n_col) in
          scale_row mat2 n_row (1. /. pivot);
          for i = 1 to num_row do
            if i <> n_row then sub_mult mat2 i n_row (get_elt mat2 (i, n_col))
          done;
          row_reduce_h (n_row + 1) (n_col + 1) mat2
  in
  (* Copies the matrix *)
  let (n, p), m = mat in
  let dim, mat_cp = empty n p in
  for i = 0 to n - 1 do
    for j = 0 to p - 1 do
      mat_cp.(i).(j) <- m.(i).(j)
    done
  done;
  let _ = row_reduce_h 1 1 (dim, mat_cp) in
  (dim, mat_cp)

let inverse (mat : matrix) : matrix =
  let (n, p), _m = mat in
  if n = p then (
    (* create augmented matrix *)
    let augmented = empty n (2 * n) in
    for i = 1 to n do
      let dim, col = get_column mat i in
      let arr = Array.make n 0. in
      assert (dim = n);
      arr.(i - 1) <- 1.;
      set_column augmented i col;
      set_column augmented (n + i) arr
    done;
    let augmented' = row_reduce augmented in
    (* create the inverted matrix and fill in with appropriate values *)
    let inverse = empty n n in
    for i = 1 to n do
      let dim, col = get_column augmented' (n + i) in
      let _ = assert (dim = n) in
      let _ = set_column inverse i col in
      ()
    done;
    inverse )
  else raise NonSquare

let print (m : matrix) : unit =
  let ((_,_), mat) = m in
  Array.iter (fun ival ->
    Printf.printf "[ ";
    Array.iter (fun jval ->
        Printf.printf "%f " jval) ival;
        Printf.printf "]\n") mat

let _ =
  let matrix =
    Array.init 100 (fun _ -> Array.init 100 (fun _ -> Random.float 100.))
  in
  let inv_matrix = inverse ((Array.length matrix, Array.length matrix.(0)), matrix) in
  (* print ((3, 3), matrix); *)
  (* Printf.printf "\n"; *)
  print inv_matrix