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
      | [|_;host;port;user;password|] ->  Unix.inet_addr_of_string host, int_of_string port,user,password
      | _ -> failwith "bad command line"
    in
    let t = Auto_ftp.connect ~host ~port ~user ~password in
      Auto_ftp.loop t
  with
    | e -> printf "%s\n" (Printexc.to_string e) ; usage () ; exit 1
