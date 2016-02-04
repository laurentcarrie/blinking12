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
    let opt = OptParse.OptParser.make () in

    let opt_host = 
      let o = OptParse.Opt.value_option "" None (fun a -> a) (fun e _ -> Printexc.to_string e) in
      let () = OptParse.OptParser.add opt ~long_name:"hostname" ~help:"hostname" o in
	o
    in
	
    let opt_user = 
      let o = OptParse.Opt.value_option "" None (fun a -> a) (fun e _ -> Printexc.to_string e) in
      let () = OptParse.OptParser.add opt ~long_name:"user" ~help:"user" o in 
	o
    in

    let opt_port = 
      let o = OptParse.Opt.value_option "" None (fun a -> int_of_string a) (fun e _ -> Printexc.to_string e) in
      let () = OptParse.OptParser.add opt ~long_name:"port" ~help:"port" o in
	o 
    in

    let opt_script = 
      let o = OptParse.Opt.value_option "" None (fun a -> a) (fun e _ -> Printexc.to_string e) in
      let () = OptParse.OptParser.add opt ~long_name:"script" ~help:"script file" o in
	o 
    in

    let opt_echo = 
      let o = OptParse.StdOpt.store_true () in
      let () = OptParse.OptParser.add opt ~long_name:"echo" ~help:"echo" o in
	o
    in
      
    let _ = OptParse.OptParser.parse_argv opt in

    let host_string = OptParse.Opt.get opt_host in
    let port = OptParse.Opt.get opt_port in
    let user = OptParse.Opt.get opt_user in
    let script = OptParse.Opt.opt opt_script in
      
    let host = Unix.gethostbyname host_string in
    let addr = host.Unix.h_addr_list.(0) in
    let password = Auto_ftp.retrieve_password ~host:host_string ~port ~user in
      
    let echo = OptParse.Opt.get opt_echo in
      
    let _ = Auto_ftp.echo echo in
    let t = 
      try 
	Auto_ftp.connect ~host:addr ~port ~user ~password 
      with
	| Auto_ftp.Connection_failed -> Auto_ftp.cancel_password ~host:host_string ~port ~user ; raise Auto_ftp.Connection_failed
	    
    in
      match script with
	| None -> Interactive.interactive_loop t read_line
	| Some f -> (
	    let fin = open_in f in
	      Interactive.interactive_loop t ( fun () -> input_line fin )
	  )
  with
    | e -> printf "%s\n" (Printexc.to_string e) ; usage () ; exit 1
	
