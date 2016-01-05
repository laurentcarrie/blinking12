open Printf
open ExtString

type t = {
  fin : in_channel ;
  fout : out_channel ;
  host : Unix.inet_addr ;
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
  let () = fprintf t.fout "PASV\n" ; flush t.fout in
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

let get_file t filename = (
  let (fin,fout) = streams_for_pasv t in
  let () = fprintf t.fout "RETR %s\n" filename ; flush t.fout in
  let line = input_line t.fin in 
  let () = printf "%s\n" line ; flush stdout ; in
  let fwrite = open_out_bin filename in
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

let list t = (
  let (fin,fout) = streams_for_pasv t in
  let () = fprintf t.fout "LIST\n" ; flush t.fout in
  let _ = input_line t.fin in
  let rec r () =
    try
      let line = input_line fin in
	printf "%s\n" line ; flush stdout ;
	r ()
    with
      | End_of_file -> printf "EOF\n" ; flush stdout ;  ()
  in
    r()
)

let command t has_data (args:string list) = (
  let (fin,fout) = streams_for_pasv t in
  let () = fprintf t.fout "%s\n" (String.join " " args) ; flush t.fout in
  let line = input_line t.fin in 
  let () = printf "%s\n" line ; flush stdout ; in
  let max = 1024 in
  let buffer = String.create max in
  let rec r () =
    try
      let nb = input fin buffer 0 max in
      output stdout buffer 0 nb ;
      if nb=0 then () else r ()
    with
    | End_of_file ->printf "EOF\n" ; flush stdout ;  ()
  in
  let () = if has_data then r() else () in
  let () = Unix.shutdown_connection fin in 
  close_in fin
)


let help () = 
  printf "
help       : this help
list       : list files in remote directory
pwd        : print remote current working directory
cwd <arg>  : change remote working direcytory
" ; flush stdout

let main host port user password  = (
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
  ] in
  let () = List.iter ( fun c ->
    fprintf t.fout "%s\n" c ; flush fout ;
    let _ = read () in ()
  ) commands in

  let rec r () = 
    let () = printf ">" ; flush stdout ; in
    let line = read_line () in
    let () = match (String.nsplit line " ")  with
      | ["ls"] -> command t true ["LIST"]
      | ["ls";d] -> command t true ["LIST";d]
      | ["list"] -> command t true ["LIST"]
      | ["list";d] -> command t true ["LIST";d]
      | ["help"] -> help ()
      | ["pwd"] -> command t false ["PWD"]
      | ["cd";d] -> command t false ["CWD";d]
      | ["cwd";d] -> command t false ["CWD";d]
      | ["get";filename] -> get_file t filename
      | s ->  printf "->??? %s\n" line ; flush stdout ;
    in
      r ()
  in
    r()
)
  
let usage () =
  printf "command host port\n"


let _ = 
  try
    let (host,port,user,password) = match Sys.argv with
      | [|_;host;port;user;password|] ->  Unix.inet_addr_of_string host, int_of_string port,user,password
      | _ -> failwith "bad command line"
    in
      main host port user password
  with
    | e -> printf "%s\n" (Printexc.to_string e) ; usage () ; exit 1
