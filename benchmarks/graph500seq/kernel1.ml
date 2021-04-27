(*Kernel 1 is basic construction of adjacency HashMap for undirected graphs 
which is corresponding to sparse graph implementation. INPUTS : ijw and m which has been 
derived from kronecker product*)

(*(*<-------OCaml Kernel 1 inspired from https://graph500.org/?page_id=12---------->
Written by support of PRISM Lab, IIT Madras and OCaml Labs*)*)

let scale = try int_of_string Sys.argv.(1) with _ -> 12

let edgefactor = try int_of_string Sys.argv.(2) with _ -> 10

(*This basically sorts the list in a way that (startVertex, endVertex), 
startVertex > endVertex.
It removes the self loops from ijw*)
let sort_vertice_list ar new_ar index =
  let rec sort_vertice_list ar maximum new_ar index =
    if index = -1 then (new_ar, int_of_float maximum)
    else if ar.(0).(index) > ar.(1).(index) then
      sort_vertice_list ar
        (max maximum ar.(0).(index))
        (Array.append new_ar
           [|[| ar.(0).(index); ar.(1).(index); ar.(2).(index) |]|])
        (index - 1)
    else if ar.(0).(index) = ar.(1).(index) then
      sort_vertice_list ar (max maximum ar.(0).(index)) new_ar (index - 1)
    else
      sort_vertice_list ar
        (max maximum ar.(1).(index))
        (Array.append new_ar
           [|[| ar.(1).(index); ar.(0).(index); ar.(2).(index) |]|])
        (index - 1)
  in
  sort_vertice_list ar 0. new_ar index

(*This is basically the construction of adj matrix [row][col], 
just in case dense graphs are being tested. All the kernels further though 
use HashMap, and thus would require changes*)

(*let constructionAdjMatrix list maxLabel = let matrix = Array.make_matrix
maxLabel maxLabel 0. in let rec fillMatrix matrix list = match list with [] ->
matrix | head::tail -> let _ = matrix.(int_of_float(List.nth head
0)).(int_of_float(List.nth head 1)) <- (List.nth head 2) in   let _ =
matrix.(int_of_float(List.nth head 1)).(int_of_float(List.nth head 0)) <-
(List.nth head 2) in fillMatrix matrix tail in fillMatrix matrix list ;;*)

(*Adding Edge adds the edge to HashMap for undirected graphs, where the binding 
is between index and the list (endVertex, weight) *)

let add_edge start_vertex end_vertex weight hash_table =
  if Hashtbl.mem hash_table start_vertex = false then
    Hashtbl.add hash_table start_vertex [ (end_vertex, weight) ]
  else
    Hashtbl.replace hash_table start_vertex
      (Hashtbl.find hash_table start_vertex @ [ (end_vertex, weight) ])

(*The two functions constructionAdjHash and kernel1 are the main 
functions driving all the other functions.*)
let rec construction_adj_hash ar hash_table index =
  if index = -1 then hash_table
  else
    let start_vertex = int_of_float ar.(index).(0)
    and end_vertex = int_of_float ar.(index).(1)
    and weight = ar.(index).(2) in
    add_edge start_vertex end_vertex weight hash_table;
    add_edge end_vertex start_vertex weight hash_table;
    construction_adj_hash ar hash_table (index - 1)

let rec adjust_for_all_vertices adj_matrix size index =
  if index = size then adj_matrix
  else if Hashtbl.mem adj_matrix index = true then
    adjust_for_all_vertices adj_matrix size (index + 1)
  else
    let _ = Hashtbl.add adj_matrix index [] in
    adjust_for_all_vertices adj_matrix size (index + 1)

let rec read_file file ijw =
  try
    match Some (input_line file) with
    | None -> ijw
    | Some line -> (
        match List.rev (String.split_on_char ',' line) with
        | [] -> read_file file ijw
        | _ :: tail ->
            let list =
              Array.of_list (List.map float_of_string (List.rev tail))
            in
            let ijw = Array.append ijw [| list |] in
            read_file file ijw )
  with End_of_file ->
    let _ = close_in file in
    ijw

let compute_number scale edgefactor =
  let n = int_of_float (2. ** float_of_int scale) in
  let m = edgefactor * n in
  (n, m)

let kernel1 ijw m =
  let ar, maximum_edge_label = sort_vertice_list ijw [||] (m - 1) in
  let hash_table = Hashtbl.create (maximum_edge_label + 1) in
  let adj_matrix = construction_adj_hash ar hash_table ( Array.length ar - 1) in
  let adj_matrix = adjust_for_all_vertices adj_matrix (maximum_edge_label + 1) 0 in
  let _ = Printf.printf "%d" maximum_edge_label in
  (adj_matrix, maximum_edge_label + 1)

let link_kronecker () =
  let file = open_in "kronecker.txt" in
  let ijw = read_file file [||] in
  (*let _ = Array.map (Printf.printf "%f" ) ijw.(0) in*)
  let adjMatrix =
    kernel1 ijw (snd (compute_number scale edgefactor))
  in
  adjMatrix
