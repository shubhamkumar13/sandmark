(*Kronecker is using the following algorithm : 
 Function Kronecker generator(scale, edgefactor) :
 	N = 2^scale
 	M = edgefactor * N (No of edges)
 	[A,B,C] = [0.57, 0.19, 0.19]
 	ijw = {	{1,1,1,1,1,...Mtimes};
 			{1,1,1,1,1...Mtimes};
 			{1,1,1,1,1...Mtimes};
 			}
 	ab = A + B;
  	c_norm = C/(1 - (A + B));
  	a_norm = A/(A + B);
  	for i in (0, scale) :
  		ii_bit = rand(1,M) > ab;
  		jj_bit = rand (1, M) > ( c_norm * ii_bit + a_norm * not (ii_bit) );(not a: a xor 0)
  		ijw(1:2,:) = ijw(1:2,:) + 2^(ib-1) * [ii_bit; jj_bit];
  	ijw(3,:) = unifrnd(0, 1, 1, M);//produce values from 0 to 1 for 1*M array.
  	
  	p = randperm (N);	
  	ijw(1:2,:) = p(ijw(1:2,:));
  	p = randperm (M);
  	ijw = ijw(:, p);
  	ijw(1:2,:) = ijw(1:2,:) - 1;
	Here, the labels are from 0 to N-1.
*)

(*(*<-------OCaml Kronecker Kernel inspired from https://graph500.org/?page_id=12---------->
Written by support of PRISM Lab, IIT Madras and OCaml Labs*)*)

let scale = try int_of_string Sys.argv.(1) with _ -> 2

let edgefactor = try int_of_string Sys.argv.(2) with _ -> 1

let rec random_weight_gen len ar =
  if len = 0 then ar
  else random_weight_gen (len - 1) (Array.append ar [| Random.float 1. |])

let rec generate_ii_bit_array ar index ab =
  if index = 0 then ar
  else if Random.float 1. > ab then
    generate_ii_bit_array (Array.append ar [| 1. |]) (index - 1) ab
  else generate_ii_bit_array (Array.append ar [| 0. |]) (index - 1) ab

let rec generate_jj_bit_array ar ii_bit m a_norm c_norm index =
  if index = m then ar
  else
    let h = ii_bit.(index) in
    if
      Random.float 1.
      > (c_norm *. h) +. (a_norm *. float_of_int (int_of_float h lxor 1))
    then
      generate_jj_bit_array
        (Array.append ar [| 1. |])
        ii_bit m a_norm c_norm (index + 1)
    else
      generate_jj_bit_array
        (Array.append ar [| 0. |])
        ii_bit m a_norm c_norm (index + 1)

let rec modify_row_i_j_w kk_array index ar newAr iter m =
  if iter = m then newAr
  else
    let element =
      ar.(iter) +. ((2. ** float_of_int index) *. kk_array.(iter))
    in
    modify_row_i_j_w kk_array index ar
      (Array.append newAr [| element |])
      (iter + 1) m

let rec compare_with_pr index m n ab a_norm c_norm ijw scale =
  if index = scale then ijw
  else
    let ii_bit = generate_ii_bit_array [||] m ab in
    let jj_bit = generate_jj_bit_array [||] ii_bit m a_norm c_norm 0 in
    let first_row_i_j_w = modify_row_i_j_w ii_bit index ijw.(0) [||] 0 m in
    let second_row_i_j_w = modify_row_i_j_w jj_bit index ijw.(1) [||] 0 m in
    let ijw =
      Array.append
        (Array.append [| first_row_i_j_w |] [| second_row_i_j_w |])
        [| ijw.(2) |]
    in
    compare_with_pr (index + 1) m n ab a_norm c_norm ijw scale

let permute list =
  let list = List.map (fun x -> (Random.bits (), x)) list in
  let list = List.sort compare list in
  List.map (fun x -> snd x) list

let transpose ar newAr =
  for i = 0 to Array.length ar - 1 do
    for j = 0 to Array.length ar.(0) - 1 do
      !newAr.(j).(i) <- ar.(i).(j)
    done
  done;
  !newAr

let compute_number scale edgefactor =
  let n = int_of_float (2. ** float_of_int scale) in
  let m = edgefactor * n in
  (n, m)

let write_file ijw file =
  let rec write_file ijw file index =
    if index = Array.length ijw then exit 0
    else
      let _ = Array.iter (Printf.fprintf file "%f, ") ijw.(index) in
      let _ = Printf.fprintf file "\n" in
      write_file ijw file (index + 1)
  in
  write_file ijw file 0

let kronecker scale edgefactor =
  let n, m = compute_number scale edgefactor in
  let a, b, c = (0.57, 0.19, 0.19) in
  let ijw = Array.make_matrix 3 m 1. in
  let ab = a +. b in
  let c_norm = c /. (1. -. (a +. b)) in
  let a_norm = a /. (a +. b) in
  let ijw = compare_with_pr 0 m n ab a_norm c_norm ijw scale in
  let third_row = random_weight_gen m [||] in
  let first_row_permute = Array.of_list (permute (Array.to_list ijw.(0))) in
  let second_row_permute = Array.of_list (permute (Array.to_list ijw.(1))) in
  let ijw =
    Array.append
      (Array.append [| first_row_permute |] [| second_row_permute |])
      [| third_row |]
  in
  let ar = Array.to_list (transpose ijw (ref (Array.make_matrix m 3 1.))) in
  let ijw = transpose (Array.of_list (permute ar)) (ref ijw) in
  if Sys.file_exists "kronecker.txt" then Sys.remove "kronecker.txt";
  let file = open_out "kronecker.txt" in
  let _ = write_file ijw file in
  let _ = close_out file in
  ijw

let _ = kronecker scale edgefactor
