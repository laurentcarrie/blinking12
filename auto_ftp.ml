open Printf
open ExtList
open ExtString

type t = {
  fin : in_channel ;
  fout : out_channel ;
  host : Unix.inet_addr ;
}

type file = {
  name : string ;
  access : string ;
  group : string ;
  is_directory : bool ;
}

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


let put_file t local_filename distant_filename = (
  let (fin,fout) = streams_for_pasv t in
  let () = fprintf t.fout "STOR %s\r\n" distant_filename ; flush t.fout in
  let line = input_line t.fin in 
  let () = printf "%s\n" line ; flush stdout ; in
  let fread = open_in_bin local_filename in
  let max = 1024 in
  let buffer = String.create max in
  let rec r () =
    try
      let nb = input fread buffer 0 max in
	if nb=0 then ( close_out fout ; () ) else (
	  output fout buffer 0 nb ;
	  r ()
	)
    with
      | End_of_file -> failwith "bad end"
  in
  let () = r() in
  let line = input_line t.fin in 
  let () = printf "%s\n" line ; flush stdout ; in
    ()
)
let get_file t distant_filename local_filename = (
  let (fin,fout) = streams_for_pasv t in
  let () = fprintf t.fout "RETR %s\r\n" distant_filename ; flush t.fout in
  let line = input_line t.fin in 
  let () = printf "%s\n" line ; flush stdout ; in
  let fwrite = open_out_bin local_filename in
  let max = 1024 in
  let buffer = String.create max in
  let rec r () =
    try
      let nb = input fin buffer 0 max in
	if nb=0 then ( close_out fwrite ; () ) else (
	  output fwrite buffer 0 nb ;
	  r ()
	)
    with
      | End_of_file -> failwith "bad end"
  in
  let () = r() in
  let line = input_line t.fin in 
  let () = printf "%s\n" line ; flush stdout ; in
    ()
)


let command t has_data (args:string list) = (
  let (fin,fout) = streams_for_pasv t in
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
	output stdout buffer 0 nb ; 
	if nb=0 then acc else r acc
    with
      | End_of_file ->printf "EOF\n" ; flush stdout ;  acc
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

let list t = (
  let data = command t true ["LIST"] in
  let data = String.nsplit data "\n" in
  let data = List.filter ( fun line -> not (String.starts_with line "total") ) data in
  let rec strip data =
    let (changed,data) = String.replace ~str:data ~sub:"  " ~by:" " in
      if changed then strip data else data
  in
  let data = List.map ( fun line -> strip line ) data in
  let data = List.filter ( fun line -> String.strip line <> "" ) data in

    List.map ( fun line ->
      let data = String.nsplit line " " in
      let (access,group,size,name) = match data with
	| access::_::group::size::d1::d2::d3::tl -> access,group,size,(String.join " " tl)
	| _ -> 	( let n = List.length data in failwith ("list, " ^ (string_of_int n) ^ " could not match : '" ^ line ^ "'"))
      in
	{
	  name=name ;
	  access=access ;
	  group=group ;
	  is_directory = String.get access 0 = 'd' ;
	}
    ) data
)

let help () = 
  printf "
help       : this help
list       : list files in remote directory
pwd        : print remote current working directory
cwd <arg>  : change remote working direcytory
" ; flush stdout

let connect ~host ~port ~user ~password  = (
  let (addr:Unix.sockaddr) = Unix.ADDR_INET ( host , port ) in
  (* let (socket:Unix.file_descr) = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in *)
  (* let () = Unix.connect socket addr in*)
  let (fin,fout) = Unix.open_connection addr in
  let t = { fin=fin ; fout=fout ; host=host } in

  let rec read () =
    try
      let line = input_line t.fin in
	printf "--> %s\n" line  ; flush stdout ; line
    with
      | End_of_file -> ""
  in
  let commands = [
    sprintf "USER %s" user ;
    sprintf "PASS %s" password  ; 

    "TYPE I" ;
    "STRU F" ;
    "MODE S" ;
    (* "SYST" ; *)
  ] in
  let () = List.iter ( fun c ->
    fprintf t.fout "%s\r\n" c ; flush fout ;
    let _ = read () in ()
  ) commands in

    t
)

let print_list t = (
  let data = list t in
  let data = List.sort ~cmp:( fun f1 f2 ->
    match f1.is_directory,f2.is_directory with
      | true,true 
      | false,false ->	  String.compare f1.name f2.name
      | true,false -> (-1)
      | false,true -> (+1)
  ) data in
    List.iter ( fun f ->
      printf "%s %s\n" ( if f.is_directory then "d" else " ") f.name 
    ) data ;
    flush stdout
)

let interactive_loop t = (
  let rec r () = 
    let () = printf ">" ; flush stdout ; in
    let line = read_line () in
    let () = match (String.nsplit line " ")  with
      | ["list"] 
      | ["ls"] -> print_list t 
(*
      | ["ls";d] -> let _ = list t "" in ()
      | ["list";d] -> let _ = list t "" in ()
*)
      | ["help"] -> help () 
      | ["pwd"] -> let ret = pwd t in printf ">>%s\n" ret ; ()
      | ["cd";d] -> let _ = command t false ["CWD";d] in ()
      | ["cwd";d] -> let _ = command t false ["CWD";d] in ()
      | ["get";filename] -> let _ = get_file t filename filename in ()
      | ["get";distant_filename;local_filename] -> let _ = get_file t distant_filename local_filename in ()
      | ["put";filename] -> let _ = put_file t filename filename in ()
      | ["put";local_filename;distant_filename] -> let _ = put_file t local_filename distant_filename in ()
      | [a;b] ->  let _ = command t false [a;b]  in ()
      | _ -> printf "->??? %s\n" line ; flush stdout ;
    in
      r ()
  in
    r()
)
  
