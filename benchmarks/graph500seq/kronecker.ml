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

module Int = struct
include Int
  let exp : int -> int -> int = fun a n -> 
    Array.fold_left (fun acc x -> acc * x) 1 (Array.make n a)
end

let scale = try int_of_string Sys.argv.(1) with _ -> 2

let edgefactor = try int_of_string Sys.argv.(2) with _ -> 1

(* let print_arr_1 = fun arr -> 
    Array.iter (fun x -> 
      Printf.printf "%f "x) arr;
    Printf.printf "\n"

let print_arr_2 = fun arr -> 
  Array.iter (fun row -> print_arr_1 row) arr

let print_arr_3 = fun tens ->
  Array.iter (fun arr -> print_arr_2 arr) tens  *)

let compare_with_pr m ab a_norm c_norm scale =
  let ijw = Array.make_matrix 3 m 1. in
  for index = 0 to pred scale do
    let ii_bit = 
      Array.init m (fun _ -> if Random.float 1. > ab then 1. else 0.) in
    let jj_bit = 
        Array.init m (fun i -> 
            let h = (c_norm *. ii_bit.(i)) +. (a_norm *. float_of_int (int_of_float (ii_bit.(i)) lxor 1)) in
            if Random.float 1. > h then 1.
            else 0.
          ) in
    ijw.(0) <- Array.init m (fun i -> ijw.(0).(i) +. ((2. ** float_of_int index) *. ii_bit.(i)));
    ijw.(1) <- Array.init m (fun i -> ijw.(1).(i) +. ((2. ** float_of_int index) *. jj_bit.(i)));
    done;
  ijw

let permute (arr : 'a array) : unit =
  let new_arr = Array.map (fun x -> (Random.bits (), x)) arr in
  Array.sort compare new_arr;
  (* print_arr_1 (Array.map snd new_arr); *)
  Array.iteri (fun i (_, x) -> arr.(i) <- x) new_arr

let transpose old_arr new_arr =
  (* Printf.printf "(%d, %d)\n" (Array.length arr) (Array.length arr.(0)); *)
  for i = 0 to Array.length old_arr - 1 do
    for j = 0 to Array.length old_arr.(0) - 1 do
      (* Printf.printf "(%d, %d)\n" i j; *)
      new_arr.(j).(i) <- old_arr.(i).(j);
    done
  done

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
  let m = edgefactor * (Int.exp 2 scale) in
  let a, b, c = (0.57, 0.19, 0.19) in
  let ab = a +. b in
  let c_norm = c /. (1. -. (a +. b)) in
  let a_norm = a /. (a +. b) in
  let ijw = compare_with_pr m ab a_norm c_norm scale in
  (* print_arr_2 ijw; *)
  (* print_arr_1 third_row; *)
  permute ijw.(0);
  (* print_arr_1 ijw.(0); *)
  permute ijw.(1);
  (* print_arr_1 ijw.(1); *)
  ijw.(2) <- Array.init m (fun _ -> Random.float 1.);
  (* print_arr_2 ijw; *)
  let temp_arr = Array.make_matrix m 3 1. in 
  transpose ijw temp_arr;
  permute temp_arr;
  transpose temp_arr ijw;
  (* print_arr_2 ijw; *)
  if Sys.file_exists "kronecker.txt" then Sys.remove "kronecker.txt";
  let file = open_out "kronecker.txt" in
  let _ = write_file ijw file in
  let _ = close_out file in
  ijw

let _ = kronecker scale edgefactor
