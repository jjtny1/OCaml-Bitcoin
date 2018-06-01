let hash s1 = Hashtbl.hash s1

let concat s1 s2 = "(" ^ string_of_int s1 ^ ":" ^ string_of_int s2 ^ ")"

let compute_merkle_level level = 
	let rec aux ac = function
		| [] -> ac
		| [h] -> (hash (concat h h)) :: ac
		| a :: b :: t -> aux ((hash (concat a b)) :: ac) t
	in

	List.rev (aux [] level)

let merkle_root li =
	let rec aux level = match (compute_merkle_level level) with
		| [] -> hash ""
		| [h] -> h
		| v -> aux v
	in

	let hashed_li = List.fold_left (fun ac h -> (hash h) :: ac) [] li in
	aux (List.rev hashed_li) 

let merkle_proof elem_index li =
	let get_sibling index li =
		let bit, sibling_index = if index mod 2 = 0 then (1, index+1) else (0, index-1) in
		(bit, List.nth li (min ((List.length li)-1) sibling_index))
	in

	let rec aux index level ac = 
		(* Add the sibling of the current level to ac *)
		let new_ac = (get_sibling index level) :: ac in
		match (compute_merkle_level level) with
		| [] | [_] -> new_ac
		| next_level -> aux (index/2) next_level new_ac
	in

	let hashed_li = List.fold_left (fun ac h -> (hash h) :: ac) [] li in
	List.rev (aux elem_index (List.rev hashed_li) [])

let merkle_verify elem root proof =
	let rec aux ac = function
		| [] -> ac
		| (side, s_hash) :: t ->
			let cumulative_hash = if side = 0 then hash (concat s_hash ac) else hash (concat ac s_hash) in
			aux cumulative_hash t
	in

	let elem_hash = hash elem in
	root = aux elem_hash proof 

