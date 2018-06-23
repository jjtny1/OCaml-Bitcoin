type 'a merkle_tree = { data : 'a list; levels : int list list; root : int}

let hash s1 = Hashtbl.hash s1

let concat s1 s2 = "(" ^ string_of_int s1 ^ ":" ^ string_of_int s2 ^ ")"

let compute_merkle_level level = 
	let rec aux ac = function
		| [] -> ac
		| [h] -> (hash (concat h h)) :: ac
		| a :: b :: t -> aux ((hash (concat a b)) :: ac) t
	in

	List.rev (aux [] level)

let get_sibling index li =
	let bit, sibling_index = if index mod 2 = 0 then (1, index+1) else (0, index-1) in
	(bit, List.nth li (min ((List.length li)-1) sibling_index))

let create_merkle_tree li =
	let rec aux ac level = match level with
		| [] -> (hash 0, [])
		| [root] as top -> (root, top :: ac)
		| v -> aux (level :: ac) (compute_merkle_level level)
	in

	let hashed_li = List.fold_left (fun ac h -> (hash h) :: ac) [] li in
	let (root, levels) = aux [] (List.rev hashed_li) in
	{ data = li; levels = List.rev levels; root = root }

let add_element merkle_tree elem =
	let rec replace_elem index elem li = match li with
		| [] -> [elem]
		| h :: t -> if (index = 0) then elem :: t else h :: replace_elem (index-1) elem t
	in

	let rec aux (new_levels,current_hash,index) level = match level with
		| [] -> ([], hash [], 0)
		| _ -> 
			let new_level = replace_elem index current_hash level in
			let (side, sibling) = get_sibling index new_level in
			let new_hash = if side = 0 then hash (concat sibling current_hash) else hash (concat current_hash sibling) in
			(new_level :: new_levels, new_hash, index/2)
	in

	let rec compress_to_root new_levels = match new_levels with
		| [] -> new_levels
		| level :: _ ->
			match level with
			| [] | [_] -> new_levels
			| v -> compress_to_root ((compute_merkle_level level) :: new_levels)
	in

	let elem_hash = hash elem in
	let index = (List.length merkle_tree.data) in
	let (new_levels, root, _) = List.fold_left aux ([], elem_hash, index) merkle_tree.levels in
	{ data=merkle_tree.data@[elem]; levels = List.rev (compress_to_root new_levels); root=root }

let merkle_proof merke_tree elem_index =
	let get_level_proof (index, ac) level = match level with
		| [] -> (0, [])
		| [root] -> (index, ac)
		| _ -> (index/2, (get_sibling index level) :: ac)
	in

	let (_, proof) = List.fold_left get_level_proof (elem_index, []) merke_tree.levels in

	List.rev proof

let merkle_verify elem root proof  =
	let rec aux ac = function
		| [] -> ac
		| (side, s_hash) :: t ->
			let cumulative_hash = if side = 0 then hash (concat s_hash ac) else hash (concat ac s_hash) in
			aux cumulative_hash t
	in

	let elem_hash = hash elem in
	root = aux elem_hash proof 
