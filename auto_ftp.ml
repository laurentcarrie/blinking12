open Printf
open ExtList
module S = String
open ExtString

let (//) = Filename.concat

exception Connection_failed
exception Password_too_old

type server = 
    | VsFTPd
    | Core_FTP
    | FTP_android
    | Unknown

type t = {
  fin : in_channel ;
  fout : out_channel ;
  host : Unix.inet_addr ;
  server : server ;
}

type file = {
  name : string ;
  access : string ;
  group : string ;
  is_directory : bool ;
}

type diff_status = 
    | Identical
    | Different
    | Only_remote
    | Only_local

let log_print = ref None


let log fs =
  (match !log_print with
    (* | None -> ksprintf ( print_endline ) *)
    | None -> ksprintf ( fun _ -> ()) fs
    | Some f -> ksprintf f fs
  ) 

let int_of_string s = 
  try
    int_of_string s
  with
    | e -> printf "could not convert to int : '%s'\n" s ; flush stdout ; raise e
  

let port_of_answer line = (
  let reg = Str.regexp "227 Entering Passive Mode (.+,.+,.+,.+,\\(.+\\),\\(.+\\))." in
    if Str.string_match reg line 0 then (
      let port1 = int_of_string (Str.matched_group 1 line) in
      let port2 = int_of_string (Str.matched_group 2 line) in
	port1*256+port2
    ) else (
      let msg = sprintf "cannot match '%s'" line in
	failwith msg
    )
)


let streams_for_pasv t = (
  let () = fprintf t.fout "PASV\r\n" ; flush t.fout in
  let port = 
    let rec r () = 
      let line = input_line t.fin in
	try
	  port_of_answer line 
	with
	  | e -> r ()
    in
      r ()
  in
  let (addr:Unix.sockaddr) = Unix.ADDR_INET ( t.host , port ) in
  let (fin,fout) = Unix.open_connection addr in
  (* let () = Unix.set_nonblock (Unix.descr_of_out_channel fout) in   *)
    (fin,fout)
)

let echo b = (
  let previous = match !log_print with | None -> false  | Some _ -> true in
    (
      if b then (
	log_print := Some ( fun s -> printf "%s" s ; flush stdout )  
      )
      else
	log_print := None
    ) ;
    previous
)

let mkdir t filename = (
  let () = fprintf t.fout "MKD %s\r\n" filename ; flush t.fout ; in
  let line = input_line t.fin in
  let () = log "%s\n" line in 
    ()
)

let sha1_of_file path = (
  let hash = Cryptokit.Hash.sha256 () in
  let fread = open_in_bin path in
  let max = 1024 in
  let buffer = S.create max in
  let rec r () =
    try
      let nb = input fread buffer 0 max in
	if nb=0 then ( close_in fread ; () ) else (
	  hash#add_substring buffer 0 nb ;
	  r ()
	)
    with
      | End_of_file -> failwith "bad end"
  in
  let () = r () in
  let b64 = Cryptokit.Base64.encode_compact () in
  let () = b64#put_string hash#result in
  let () = b64#finish in 
  let data = b64#get_string in
    data
)

let walk_in_distant_file t distant_filename (acc:'a) (walk:'a -> string -> 'a) : 'a = (
  
  let (fin,fout) = streams_for_pasv t in
  let command = sprintf "RETR %s\r\n" distant_filename in
  let () = log "%s" command in
  let () = fprintf t.fout "%s" command ; flush t.fout in
  let line = input_line t.fin in 
  let () = log "%s\n" line in
  let () = 
    let code = int_of_string (fst(String.split line " ")) in
      match code with
	| 550 -> failwith "retrieve file failed"
	| 150 -> ()
	| 226 -> ()
	| l -> printf "code %d\n" l ; failwith (sprintf "unnamaged return code : %d" l)
  in
  let max = 1024 in
  let buffer = String.create max in
  let rec r acc=
    try
      let nb = input fin buffer 0 max in
	if nb=0 then (acc) else (
	  let acc = walk  acc (String.sub buffer 0 nb) in
	    r acc
	)
    with
      | End_of_file -> failwith "bad end"
  in
  let acc = r acc  in
  let _ = input_line t.fin in 
(*
  let () = close_in fin in
  let () = close_out fout in
*)
    acc
)

let get_file t distant_filename local_filename = (
  try
    let (fin,fout) = streams_for_pasv t in
    let command = sprintf "RETR %s\r\n" distant_filename in
    let () = log "%s" command in
    let () = fprintf t.fout "%s" command ; flush t.fout in
    let line = input_line t.fin in 
    let () = log "%s\n" line in 
    let () = log "writing to '%s'\n" local_filename in
    let fwrite = open_out_bin local_filename in

    let max = 1024*280 in
    let buffer = String.create max in

    let implem_iteratif = false in

    let () = if implem_iteratif then (
      let encore = ref true in
	while !encore do 
	  let nb = input fin buffer 0 max in
	    if nb=0 then ( close_out fwrite ; encore:=false ) else (
	      output fwrite buffer 0 nb ; 
	      ()
	    )
	done ;
    ) else (
      let rec r () =
	let nb = input fin buffer 0 max in
	  if nb=0 then ( close_out fwrite ; () ) else (
	    output fwrite buffer 0 nb ;
	    r ()
	  )
      in
      let () = r() in
	()
    ) in

    let line = input_line t.fin in 
    let () = log "%s\n" line in
      ()
  with
    | e -> printf "Error in get_file %s %s\n" distant_filename local_filename ; flush stdout ; raise e
)


let put_data_in_distant_file t distant_filename data = (
  try
    let (fin,fout) = streams_for_pasv t in
    let () = fprintf t.fout "STOR %s\r\n" distant_filename ; flush t.fout in
    let line = input_line t.fin in 
    let () = log "%s\n" line in
    let _ = output fout data 0 (String.length data) in
    let () = close_out fout in
      ()
  with
    | e -> printf "Erreur in put_data_in_distant_file %s\n" distant_filename ; flush stdout ; raise e
)

let mv t old_name new_name = (
  let () = fprintf t.fout "RNFR %s\r\n" old_name ; flush t.fout ; in
  let line = input_line t.fin in
  let () = log "%s\n" line in 
  let () = fprintf t.fout "RNTO %s\r\n" new_name ; flush t.fout ; in 
  let line = input_line t.fin in
  let () = log "%s\n" line in
    ()
)
 
let rec really_put_file t local_filename distant_filename = (
  try
  let () = log "really_put_file %s %s\n" local_filename distant_filename in
  let distant_filename_tmp = distant_filename ^ ".tmp" in
  let do_write () =  (
    let (fin,fout) = streams_for_pasv t in
    let () = fprintf t.fout "STOR %s\r\n" distant_filename_tmp ; flush t.fout in
    let line = input_line t.fin in 
    let () = log "%s\n" line in
    let fread = try
	open_in_bin local_filename 
      with
	| e -> printf "Error while opening %s\n" local_filename ; flush stdout ; raise e
    in
    let max = 1024 in
    let buffer = String.create max in

    let rec r () =
      let nb = input fread buffer 0 max in
	if nb=0 then ( close_out fout ; close_in fread ; () ) else (
	  output fout buffer 0 nb ;
	  r ()
	)
    in
    let () = r() in
    let () = mv t distant_filename_tmp distant_filename in
      ()
  )
  in
    do_write () 
  with
    | e -> printf "Erreur in really_put_file %s %s\n" local_filename distant_filename ; flush stdout ; raise e
)
  
let put_file_with_sha1 t local_filename distant_filename = (
  let sha1s =  (
    try 
      let walk acc s = acc ^ s in
      let data = walk_in_distant_file t ".sha1"  "" walk in
	List.map ( fun line -> let (a,b) = String.split line " " in b,a ) (String.nsplit data "\n")
    with
      | e -> log "%s\n%s\n" ".sha1 not found" (Printexc.to_string e) ; [] 
  )
  in
  let local_sha1 = sha1_of_file local_filename in
  let () = log "local_sha1 for %s = %s\n" local_filename local_sha1 in
  let do_it = 
    try
      let distant_sha1 = List.assoc distant_filename sha1s in
      let different = local_sha1 <> distant_sha1 in
      let () = log "sha1s are different ? %b\n" different in
	different
    with
      | Not_found -> log "%s" "distant sha1 not found\n" ; true
  in
  let () = if do_it then (
    let () = really_put_file t local_filename distant_filename in
    let sha1s = (distant_filename,local_sha1)::(List.remove_assoc distant_filename sha1s) in
    let data = String.join "\n" ( List.map ( fun (f,sha1) -> sprintf "%s %s" sha1 f) sha1s ) in
      put_data_in_distant_file t ".sha1" data
  )
    else (
      log "SKIPPED because same sha1 : %s -> %s\n" local_filename distant_filename 
    ) in

    ()

)

let put_file ~use_sha1 t local_filename distant_filename = (
  if use_sha1 then 
    put_file_with_sha1 t local_filename distant_filename
  else
    really_put_file t local_filename distant_filename 
)


let command t has_data (args:string list) = (
  let (fin,fout) = streams_for_pasv t in
  let () = log "%s\r\n" (String.join " " args) in
  let () = fprintf t.fout "%s\r\n" (String.join " " args) ; flush t.fout in
(*
      let line = input_line t.fin in 
      let () = printf "%s\n" line ; flush stdout ; in
*)
  let max = 1024 in
  let buffer = String.create max in
  let rec r acc =
    try
      let nb = input fin buffer 0 max in
      let acc = acc ^ (String.sub buffer 0 nb) in
	log "%s" (String.sub buffer 0 nb ) ;
	if nb=0 then acc else r acc
    with
      | End_of_file -> log "EOF\n" ;  acc
  in
  let ret = if has_data then r "" else "" in
  let () = Unix.shutdown_connection fin in 
    close_in fin ;
    ret
)

let pwd t = (
  let _ = command t false ["PWD"] in
  let line = input_line t.fin in
  let reg = Str.regexp "257 \"\\(.*\\)\".*" in
    if Str.string_match reg line 0 then
      Str.matched_group 1 line
    else
      failwith ("pwd, could not match : " ^ line)
)
  
let cwd t dir = (
  let _ = command t false ["CWD";dir] in
    ()
)

let list t dirname = (
  let () = log "list '%s'\n" dirname in
  let data = command t true ["LIST";dirname] in
  let data = String.nsplit data "\n" in
  let data = List.filter ( fun line -> not (String.starts_with line "total") ) data in
  let rec strip data =
    let (changed,data) = String.replace ~str:data ~sub:"  " ~by:" " in
      if changed then strip data else data
  in
  let data = List.map ( fun line -> strip line ) data in
  let data = List.map ( fun line -> String.strip line ) data in
  let data = List.filter ( fun line -> String.strip line <> "" ) data in

    List.map ( fun line ->
      let data = String.nsplit line " " in
      let (access,group,size,name) = 
	match t.server with
	| Unknown
	| VsFTPd -> (
	  match data with
	  | access::_::group::size::d1::d2::d3::tl -> access,group,size,(String.join " " tl)
	  | _ -> 	( let n = List.length data in failwith ("list, " ^ (string_of_int n) ^ " could not match : '" ^ line ^ "'"))
	)
	| FTP_android
	| Core_FTP -> (
	  match data with
	  | access::_::group::size::d1::d2::d3::d4::tl -> access,group,size,(String.join " " tl)
	  | _ -> 	( let n = List.length data in failwith ("list, " ^ (string_of_int n) ^ " could not match : '" ^ line ^ "'"))
	)
      in
	{
	  name=name ;
	  access=access ;
	  group=group ;
	  is_directory = String.get access 0 = 'd' ;
	}
    ) data
)

let status t path = (
  let path = if Filename.is_relative path then "." // path else path in
  let dirname = Filename.dirname path in
  let files = list t dirname in
    try
      Some ( List.find ( fun f -> 
	log "? match %s and %s\n" path (dirname//f.name) ;
	path = dirname // f.name ) files )
    with
      | Not_found -> None
)

let get_dir t distant_dir local_dir = (
  log "get_dir '%s' '%s'\n" distant_dir local_dir ;
  let rec r distant_dir local_dir = (
    let () = log "create local directory '%s'\n" local_dir in
    let () = try Unix.mkdir local_dir 0o755 with | _ -> () in
    let files = list t distant_dir in
      List.iter ( fun f ->
	let () = log "E: '%s'\n" f.name in
	if f.is_directory then (
	  r (distant_dir//f.name) (local_dir//f.name) 
	) else (
	  get_file t (distant_dir//f.name) (local_dir//f.name)
	)
      ) files
  )
  in
    r distant_dir local_dir
)

let put_dir ~use_sha1 t local_dir distant_dir  = (
  log "put_dir '%s' '%s'\n" distant_dir local_dir ;
  (* let () = get_dir t (distant_dir // ".sha1" ) (local_dir // ".sha1") in *)
  let rec r local_dir distant_dir  = (
    let () = mkdir t distant_dir in
    let files = Array.to_list ( Sys.readdir local_dir ) in
      List.iter ( fun f ->
	let () = log "E: '%s'\n" f in
	  if Sys.is_directory (local_dir//f) then (
	    r (local_dir//f)  (distant_dir//f) 
	  ) else (
	    put_file ~use_sha1 t (local_dir//f) (distant_dir//f)
	  )
      ) files
  )
  in
    r local_dir distant_dir
)
  

let nlst t dirname = (
  let data = command t true ["NLST";dirname] in
    log "%s\n" data ;
    data
)


let rm t filename = (
  let () = log ">>>%s<<<\n" filename in
  let command = sprintf "DELE %s\r\n" filename in 
  let () = log "%s" command in
  let () = fprintf t.fout "%s" command ; flush t.fout ; in
  let line = input_line t.fin in
  let () = log "%s\n" line in 
    ()
)

let cwd t dir = (
  let _ = command t false ["CWD";dir] in
    ()
)

let stat t = (
  let _ = command t false ["STAT"] in
  let rec r acc =
    try
      let line = input_line t.fin in
      let (first_word,_) = String.split line " " in
      let acc = acc ^ "\n" ^ line in
	try
	  let _ = int_of_string first_word in
	    acc 
	with
	  | _ -> r acc
    with
      | _ -> acc
  in
    r "" 
)


let rec rmdir t dirname = (
  let files = list t dirname in
  let (files,dirs) = List.partition ( fun f -> not f.is_directory ) files in
  let () = log "%d files to delete\n" (List.length files) in
  let () = log "%d dirs to delete\n" (List.length dirs) in
  let () = List.iter ( fun f ->
    let () = log "delete %s\n" ( dirname // f.name ) in
      rm t ( dirname // f.name )
  ) files in
  let () = List.iter ( fun f -> rmdir t ( dirname // f.name )) dirs in
  let command = sprintf "RMD %s\r\n" dirname in
  let () = log "%s" command in
  let () = fprintf t.fout "%s" command ; flush t.fout ; in
  let line = input_line t.fin in
  let () = log "%s\n" line in 
  let () = Option.may ( fun _ -> Unix.sleep 1 ; rmdir t dirname ) ( status t dirname ) in
    ()
)

let mv t old_name new_name = (
  let () = fprintf t.fout "RNFR %s\r\n" old_name ; flush t.fout ; in
  let line = input_line t.fin in
  let () = log "%s\n" line in 
  let () = fprintf t.fout "RNTO %s\r\n" new_name ; flush t.fout ; in 
  let line = input_line t.fin in
  let () = log "%s\n" line in
    ()
)

let connect ~host ~port ~user ~password  = (
  let (addr:Unix.sockaddr) = Unix.ADDR_INET ( host , port ) in
  (* let (socket:Unix.file_descr) = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in *)
  (* let () = Unix.connect socket addr in*)
  let (fin,fout) = Unix.open_connection addr in
  let server =
    let line = input_line fin in
    let () = log "%s\n" line in
    let reg_ftp_android = Str.regexp "220 FTPServer ready.*" in
    let reg_vsftpd = Str.regexp "220.*FTP server.*" in
    let reg_coreftp = Str.regexp "220-Core FTP Server.*" in
      if Str.string_match reg_vsftpd line 0 then ( log "server is vsFTP\n" ; VsFTPd )
      else if Str.string_match reg_coreftp line 0 then ( log "server is CoreFTP\n" ; Core_FTP )
      else if Str.string_match reg_ftp_android line 0 then ( log "server is CoreFTP\n" ; FTP_android )
      else ( log "cannot identify server, please fix that\n" ; Unknown )
  in
    
  let t = { fin=fin ; fout=fout ; host=host ; server=server ; } in

  let rec read () =
    try
      let line = input_line t.fin in
      let () = if ( String.starts_with line "530" ) then raise Connection_failed else () in
	log "--> %s\n" line  ; line
    with
      | End_of_file -> ""
  in
  let commands = [
    sprintf "USER %s" user ;
    sprintf "PASS %s" password  ; 

    "TYPE I" ;
    (* "STRU F" ;*)
    (* "MODE S" ; *)
    "SYST" ; 
  ] in
  let () = List.iter ( fun c ->
    fprintf t.fout "%s\r\n" c ; flush fout ;
    let _ = read () in ()
  ) commands in

    t
)

let dir_compare t local distant = (
  let local_files = Array.to_list (Sys.readdir local) in
  let distant_files = list t distant in
    (List.map ( fun l -> (l,Only_local)) local_files)
      @
      (List.map ( fun f -> (f.name,Only_remote)) distant_files)
)

class a_scheme =
object(self)
  method pad s i =
    for j=i to (String.length s - 1) do
      s.[i] <- '0'
    done
  method strip (s:string) =
    try
      String.index s '0'
    with
      | Not_found -> String.length s - 1
end ;;



let forge_password_filename ~host ~port ~user = (
  sprintf "%s.%d.%s" host port user 
)

let ask_password ~host ~port ~user = (
  try
    let () = printf "host:%s\nport:%d\nuser:%s\nenter password : " host port user ; flush stdout ; in
    let rec secretly_read_password acc = 
      try
	let c = input_char stdin in
	  if c = '\n' then (String.implode (List.rev acc)) else secretly_read_password(c::acc)
      with
	| End_of_file -> printf "end of file\n" ; flush stdout ; (String.implode (List.rev acc))
    in
    let term_init = Unix.tcgetattr Unix.stdin in
      (* let term_noecho = { term_init with Unix.c_echo = false } in *)
    let () = Unix.tcsetattr Unix.stdin Unix.TCSANOW { term_init with Unix.c_echo=false} in
    let password = secretly_read_password []  in
    let () = Unix.tcsetattr Unix.stdin Unix.TCSADRAIN { term_init with Unix.c_echo=true}  in 
      password
  with
    | e -> (
	let term_init = Unix.tcgetattr Unix.stdin in
	let () = Unix.tcsetattr Unix.stdin Unix.TCSADRAIN { term_init with Unix.c_echo=true}  in 
	  raise e
      )
)

type password = {
  password : string ;
  time : float ;
}


let store_password ~host ~port ~user ~filename ~key ~password = (
  (* let a_scheme = new a_scheme in *)
  let a_scheme = Cryptokit.Padding._8000 in
  let t = Cryptokit.Cipher.aes  ~pad:a_scheme key Cryptokit.Cipher.Encrypt in
  let crypted = Cryptokit.transform_string t (Marshal.to_string { password=password;time=Unix.time()} []) in
  let () = Std.output_file ~text:crypted ~filename in
    ()
)

let retrieve_password ~host ~port ~user = (
  let filename = forge_password_filename ~host ~port ~user in
  let key = "hello.world12345" in
  let () = assert(String.length key = 16) in
  let rec read_password () = 
    try
      (* let a_scheme = new a_scheme in *)
      let a_scheme = Cryptokit.Padding._8000 in
      let t = Cryptokit.Cipher.aes  ~pad:a_scheme key Cryptokit.Cipher.Decrypt in
      let crypted = Std.input_file filename in
      let data = Marshal.from_string (Cryptokit.transform_string t crypted) 0 in
      let () = store_password ~host ~port ~user ~filename ~key ~password:data.password in
	(* let () = printf "timestamp : %f (now is %f)\n" data.time (Unix.time()) in *)
      let () = if ( Unix.time() -. data.time > 600. ) then raise Password_too_old else () in
	data.password
    with
      | Password_too_old
      | _ -> ( 
	  let password = ask_password ~host ~port ~user in
	  let () = store_password ~host ~port ~user ~filename ~key ~password in read_password () 
	)
  in
    read_password ()
)
  
let cancel_password ~host ~port ~user = (
  let filename = forge_password_filename ~host ~port ~user in
    try
      Unix.unlink filename ;
      ()
    with
      | _ -> ()

)

let build_distant_sha1 t local_dirname distant_dirname = (

  let get_sha1 f = 
    let hash = Cryptokit.Hash.sha256 () in
    let rec walk () s =
      hash#add_string s ;
      ()
    in
    let () = walk_in_distant_file t f () walk in
    let b64 = Cryptokit.Base64.encode_compact () in
    let () = b64#put_string hash#result in
    let () = b64#finish in 
    let data = b64#get_string in
      data
  in
    
  let rec walk acc d = (
    (* heuristique ... *)
    let l = String.nsplit (String.strip (nlst t d)) "\n" in
    let l = List.map String.strip l in
    let () = List.iteri ( fun i s -> log "---> %d [%s]\n" i s) l in
    let acc = match l with
	| [] -> failwith "internal error, nlst returns empty"
	| hd::[] -> (
	    (* file *) 
	    let sha1 = get_sha1 d in
	      (* log "sha1 : %s\n" (get_sha1 d) ;*)
	      (sha1,d) :: acc
	  )
	| l -> (
	    let l = List.map String.strip l in
	    let l = List.filter ( fun s -> s <> "." && s <> ".." && s <> d) l in
	    let acc = List.fold_left ( fun acc s ->
	      walk acc (d//s)
	    ) acc l  in
	      acc
	  )
    in
      acc
  ) in
  let acc = walk [] distant_dirname in
  let data = String.join "\n" (List.map ( fun (sha1,path) -> sprintf "%s %s" sha1 path) acc ) in
    put_data_in_distant_file t ".sha1" data ;
    []

)
