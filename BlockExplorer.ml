
type varInt = Zero of int | FD of int | FE of int | FF of int
type txout = {value : int64; txout_script_len : varInt; txout_script : bytes}
type txin = {prev_transaction_hash : bytes; prev_txout_index : int; txin_script_length : varInt; txin_script : bytes; seq_no : int;}
type transaction = {ver : int; in_counter : varInt; inputs : txin list; out_counter : varInt; outputs : txout list; lock_time : int;}
type blockheader = {version : int; hash_prev_block : bytes; hash_merkle_root : bytes; time : int; bits : int; nonce : int;}
type block = {magic_no : int; blocksize : int; block_header : blockheader; transaction_counter : varInt; transactions : transaction list;}
type blockchain = block list

let get_int_of_varInt (v:varInt) : int = match v with
	Zero n | FD n | FE n | FF n -> n

let print_blockheader (blkhead : blockheader) :unit =

	let print_bytes (byt:Bytes.t) =
		Bytes.iter (fun c -> Printf.printf "%02x" (int_of_char c)) byt
	in

	Printf.printf "Version : %d\n" blkhead.version;

	Printf.printf "Previous Block Hash : ";
	print_bytes blkhead.hash_prev_block;
	Printf.printf "\n";

	Printf.printf "Merkle Root : ";
	print_bytes blkhead.hash_merkle_root;
	Printf.printf "\n";

	Printf.printf "Time : %d\n" blkhead.time;

	Printf.printf "Bits : %d\n" blkhead.bits;

	Printf.printf "Nonce : %d\n" blkhead.nonce

let print_transactions tx_li =

	let print_bytes (byt:Bytes.t) =
		Bytes.iter (fun c -> Printf.printf "%02x" (int_of_char c)) byt
	in

	let print_transaction_inputs inputs_li =
		let print_transaction_input txin =

			Printf.printf "Previous Transaction Hash : ";
			print_bytes txin.prev_transaction_hash;
			Printf.printf "\n";

			Printf.printf "Previous Transaction Out Index : %d\n" txin.prev_txout_index;

			Printf.printf "Transaction Input Script Length : %d\n" (get_int_of_varInt txin.txin_script_length);

			Printf.printf "Transaction Input Script : ";
			print_bytes txin.txin_script;
			Printf.printf "\n";

			Printf.printf "Sequence Number : %d\n" txin.seq_no
		in

		List.iter (fun txin -> Printf.printf "( "; print_transaction_input txin; Printf.printf " )\n") inputs_li
	in

	let print_transaction_outputs outputs_li =
		let print_transaction_output txout =
			Printf.printf "Value (in Satoshis) : %d\n" (Int64.to_int txout.value);
			Printf.printf "Transaction Output Script Length : %d\n" (get_int_of_varInt txout.txout_script_len);

			Printf.printf "Transaction Output Script : ";
			print_bytes txout.txout_script;
			Printf.printf "\n"
		in

		List.iter (fun txout -> Printf.printf "( "; print_transaction_output txout; Printf.printf " )\n") outputs_li
	in


	let print_transaction tx =
		Printf.printf "Ver : %d\n" tx.ver;
		Printf.printf "Number of Inputs : %d\n" (get_int_of_varInt tx.in_counter);

		Printf.printf "Transaction Inputs [ ";
		print_transaction_inputs tx.inputs;
		Printf.printf "]\n";

		Printf.printf "Number of Outputs : %d\n" (get_int_of_varInt tx.out_counter);

		Printf.printf "Transaction Outputs [ ";
		print_transaction_outputs tx.outputs;
		Printf.printf "]\n";

		Printf.printf "Transaction Lock Time : %d\n" tx.lock_time
	in

	List.iter (fun tx -> Printf.printf "{ "; print_transaction tx; Printf.printf " }\n") tx_li


let print_block (blk : block) : unit =
	Printf.printf "Magic No. : %d\n" blk.magic_no;
	Printf.printf "Block Size : %d\n" blk.blocksize;
	Printf.printf "Block Header : \n";
	print_blockheader blk.block_header;
	Printf.printf "Number of Transactions : %d\n" (get_int_of_varInt blk.transaction_counter);
	Printf.printf "Transactions : \n";
	print_transactions blk.transactions


let convert_endianess (le_arr:Bytes.t) : Bytes.t =
	let rec aux (new_arr:Bytes.t) (old_arr:Bytes.t) (new_arr_index:int) (ac:int) : Bytes.t =
		if ac < 0 then
			new_arr
		else
			(Bytes.set new_arr new_arr_index (Bytes.get old_arr ac);
			aux new_arr old_arr (new_arr_index + 1) (ac - 1))
	in

	aux (Bytes.make (Bytes.length le_arr) (char_of_int 0)) le_arr 0 ((Bytes.length le_arr) - 1)

let convert_bytes_to_int (arr:Bytes.t) : int =
	let rec aux (arr:Bytes.t) (arr_length:int) (arr_index:int) (int_fin:int) : int =
		if arr_index == (arr_length - 1) then
			let byte_val = Char.code (Bytes.get arr arr_index) in
			let int_fin = int_fin lor byte_val in
			int_fin
		else
			let byte_val = Char.code (Bytes.get arr arr_index) in
			let int_fin = int_fin lor byte_val in
			aux arr arr_length (arr_index + 1) (int_fin lsl 8)

	in

	aux arr (Bytes.length arr) 0 0


let create_block_from_file file =
	(*
	let set_file_position file pos = 
		seek_in file pos 
	in *)

	let read_bytes_from_file file num_bytes =
		let buf = Bytes.make num_bytes (char_of_int 0) in

		try really_input file buf 0 num_bytes; buf
		with End_of_file -> failwith "Could not read bytes"

	in

	let get_magic_no file =
		(* set_file_position file 0; *)
		convert_bytes_to_int (convert_endianess (read_bytes_from_file file 4))

	in

	let get_block_size file =
		(* set_file_position file 4; *)
		convert_bytes_to_int (convert_endianess (read_bytes_from_file file 4))
	in


	let get_block_header file =
		(* set_file_position file 8; *)
		let version = convert_bytes_to_int (convert_endianess (read_bytes_from_file file 4)) in

		let prev_block = (convert_endianess (read_bytes_from_file file 32)) in
		let merkle_root = (convert_endianess (read_bytes_from_file file 32)) in

		let time = convert_bytes_to_int (convert_endianess (read_bytes_from_file file 4)) in
		let bits = convert_bytes_to_int (convert_endianess (read_bytes_from_file file 4)) in
		let nonce = convert_bytes_to_int (convert_endianess (read_bytes_from_file file 4)) in

		{version = version; hash_prev_block = prev_block; hash_merkle_root = merkle_root; time = time; bits = bits; nonce = nonce;}

	
	in

	let get_var_int file  =
		let get_val byte = match (Bytes.get byte 0) with
			'\253' -> FD(convert_bytes_to_int (convert_endianess (read_bytes_from_file file 2)))
			|'\254' -> FE(convert_bytes_to_int (convert_endianess (read_bytes_from_file file 4)))
			|'\255' -> FF(convert_bytes_to_int (convert_endianess (read_bytes_from_file file 8)))
			|v -> if (int_of_char v) > 253 then failwith "Invalid VarInt" else Zero(convert_bytes_to_int (convert_endianess byte))
		in

		(* set_file_position file pos; *)
		get_val (read_bytes_from_file file 1)
	in



	let build_transaction file =

		let get_script file = function
			Zero to_r | FD to_r | FE to_r | FF to_r -> read_bytes_from_file file to_r (*Scripts are kept in LE order*)
		in

		let get_list_inputs file txin_counter =
			let get_txin file =  	

				let prev_transaction_hash = convert_endianess (read_bytes_from_file file 32) in
				let prev_txout_index = convert_bytes_to_int (convert_endianess (read_bytes_from_file file 4)) in
				let txin_script_length = get_var_int file in
				let script_sig = get_script file txin_script_length in
				let seq_no = convert_bytes_to_int (convert_endianess (read_bytes_from_file file 4)) in

				{prev_transaction_hash = prev_transaction_hash; prev_txout_index = prev_txout_index; txin_script_length = txin_script_length; txin_script = script_sig; seq_no = seq_no;}
			in

			let rec aux count li =
				if count = 0 then
					li
				else
					aux (count - 1) (li@[get_txin file])
			in

			(match txin_counter with 
				Zero n | FD n | FE n | FF n -> aux n [])
		in

		let get_list_outputs file tx_out_counter =
			let get_txout file =
				let value = convert_bytes_to_int (convert_endianess (read_bytes_from_file file 8)) in
				let txout_script_len = get_var_int file in
				let script_pub_key = get_script file txout_script_len in
				{value = Int64.of_int value; txout_script_len = txout_script_len; txout_script = script_pub_key}
			in

			let rec aux count li =
				if count = 0 then
					li
				else
					aux (count - 1) (li@[get_txout file])
			in

			(match tx_out_counter with 
				Zero n | FD n | FE n | FF n -> aux n [])
		in


		let t_ver_no = convert_bytes_to_int (convert_endianess (read_bytes_from_file file 4)) in
		let t_in_counter = get_var_int file in
		let li_inputs = get_list_inputs file t_in_counter in
		let tx_out_counter = get_var_int file in
		let li_ouputs = get_list_outputs file tx_out_counter in
		let lock_time = convert_bytes_to_int (convert_endianess (read_bytes_from_file file 4)) in

		{ver = t_ver_no; in_counter = t_in_counter; inputs = li_inputs; out_counter = tx_out_counter; outputs = li_ouputs; lock_time = lock_time;}

	in

	let get_transactions_list file transaction_count =
		let rec aux count li = 
			if count = 0 then
				li
			else
				aux (count - 1) (li@[build_transaction file])
		in

		(match transaction_count with 
		Zero n | FD n | FE n | FF n -> aux n [])

	in

	let magic_no = get_magic_no file in

	let blocksize = get_block_size file in

	let block_header = get_block_header file in

	let transaction_count = get_var_int file  in

	let transactions_list = get_transactions_list file transaction_count in

	{magic_no = magic_no; blocksize = blocksize; block_header = block_header; transaction_counter = transaction_count; transactions = transactions_list;}

let create_blockchain_from_file filename =

	let rec aux file current_pos total_size blkchain = 
		(* 
		Printf.printf "Current pos : %d\n" current_pos;
		Format.print_flush (); *)
		if current_pos < total_size then
			aux file (pos_in file) total_size ((create_block_from_file file)::blkchain)
		else
			List.rev blkchain
	in

	let file = open_in filename in
	let total_size = in_channel_length file in
	aux file (pos_in file) total_size []

let create_entire_blockchain directory =
	let rec copy source destination = match source with
		[] -> destination
		|h::t -> copy t (h::destination)
	in

	let build_file_name directory file_count =
		let file_name_num = Printf.sprintf "blk%05d" file_count in
		directory ^ file_name_num ^ ".dat"
	in

	let rec aux directory file_count blockchain =

		let file_name = build_file_name directory file_count in
		Printf.printf "File count is %d\n" file_count;
		Format.print_flush ();
		if Sys.file_exists file_name then
			let file_blkchain = create_blockchain_from_file file_name in
			aux directory (file_count + 1) (copy file_blkchain blockchain)
		else
			List.rev blockchain
	in

	aux directory 0 []
