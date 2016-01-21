open Printf
open ExtString


let help () = 
  printf "
help       : this help
list       : list files in remote directory
pwd        : print remote current working directory
cwd <arg>  : change remote working direcytory
" ; flush stdout

let usage () =
  printf "command host port\n"


let _ = 
  try
    let (host,port,user,password) = match Sys.argv with
      | [|_;host_string;port;user|] ->  (
	  let host = Unix.gethostbyname host_string in
	  let addr = host.Unix.h_addr_list.(0) in
	  let port = int_of_string port in
	  let password = Auto_ftp.retrieve_password ~host:host_string ~port ~user in
	    addr,port,user,password)
      | _ -> failwith "bad command line"
    in
    let _ = Auto_ftp.echo true in
    let t = Auto_ftp.connect ~host ~port ~user ~password in
      Interactive.interactive_loop t
  with
    | e -> printf "%s\n" (Printexc.to_string e) ; usage () ; exit 1
