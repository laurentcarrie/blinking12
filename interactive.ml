open Printf
open ExtList
open ExtString

let log = Auto_ftp.log

let help () = 
  printf "
help                 : this help
list                 : list files in remote directory
ls                   : list files in remote directory
pwd                  : print remote current working directory
cwd <arg>            : change remote working direcytory
rm <arg>             : remove file
mkdir <arg>          : make directory
rmdir <arg>          : remove directory (must be empty)
mv <arg1> <arg2>     : move file
put <local> <remote> : put file
get <remote> <local> : get file
" ; flush stdout

let print_list t d = (
  let data = Auto_ftp.list t d in
  let data = List.sort ~cmp:( fun f1 f2 ->
    match f1.Auto_ftp.is_directory,f2.Auto_ftp.is_directory with
      | true,true 
      | false,false ->	  String.compare f1.Auto_ftp.name f2.Auto_ftp.name
      | true,false -> (-1)
      | false,true -> (+1)
  ) data in
    List.iter ( fun f ->
      printf "%s %s\n" ( if f.Auto_ftp.is_directory then "d" else " ") f.Auto_ftp.name 
    ) data ;
    flush stdout
)

let print_compare t l d = (
  let () = log "======== COMPARE %s and %s\n" l d in
  let data = Auto_ftp.dir_compare t l d in
  let string_of_status d = match d with
    | Auto_ftp.Identical -> " = "
    | Auto_ftp.Different -> " # "
    | Auto_ftp.Only_remote -> " r "
    | Auto_ftp.Only_local -> " l "
  in
    List.iter ( fun (name,status) ->
      printf "%s %s\n" (string_of_status status) name
    ) data ;
    flush stdout
)

let help commands t _ = (
  List.iter ( fun (name,_,h) ->
    printf "%s\n   %s\n" name h
  ) commands
)

let commands : (string * (Auto_ftp.t->string list->unit) * string) list = (
  let commands = [
    "ls",(fun t l -> match l with | [] -> print_list t "." | hd::_ -> print_list t hd ), "ls distant dir" ;

    "echo",(
      fun _ args -> 
	let _= match args with 
	  | ["on"] -> let _ = Auto_ftp.echo true  in ()
	  | ["off"] -> let _ = Auto_ftp.echo false in ()
	  | _ -> log "%s" "bad arg for echo"
	in ()
    ),"set echo [on|off]" ;
    
    "pwd",(fun t _ -> let ret = Auto_ftp.pwd t in printf "%s\n" ret ; ()) , "print distant working directory" ;

    "cd",(fun t args -> 
      let () = match args with 
	| hd::[] -> Auto_ftp.cwd t hd 
	| _ -> (log "%s" "bad args for cd")
      in
	()
    ),"change working directory" ;

    
    "get",(fun t args ->
      let () = match args with
	| filename::[] -> let _ = Auto_ftp.get_file t filename filename in ()
	| distant::local::[] -> let _ = Auto_ftp.get_file t distant local in ()
	| _ -> (log "%s" "bad args for get")
      in ()
    ),"get [filename | distant_name local_name] : gets a file" ;

    "get_dir",(fun t args ->
      let () = match args with
	| dirname::[] -> let _ = Auto_ftp.get_dir t dirname dirname in ()
	| distant_dirname::local_dirname::[] -> let _ = Auto_ftp.get_dir t distant_dirname local_dirname in ()
	| _ -> (log "%s" "bad args for get_dir")
      in  ()
    ),"get_dir [dirname | distant_dirname local_dirname] : recursively gets a directory, using sha1 stored to optimize" ;

    "put_dir_no_sha1",(fun t args ->
      match args with
	| dirname::[] ->  let _ = Auto_ftp.put_dir ~use_sha1:false t dirname dirname in ()
	| local_dirname::distant_dirname::[] -> let _ = Auto_ftp.put_dir ~use_sha1:false t local_dirname distant_dirname in ()
	| _ -> (log "%s" "bad args for put_dir_no_sha1")
    ), "recursively put directory, ignoring sha1 hashes (really puts all files)" ;
						  
    "put_dir",(fun t args ->
      match args with
	| dirname::[] ->  let _ = Auto_ftp.put_dir ~use_sha1:false t dirname dirname in ()
	| local_dirname::distant_dirname::[] -> let _ = Auto_ftp.put_dir ~use_sha1:true t local_dirname distant_dirname in ()
	| _ -> (log "%s" "bad args for put_dir_no_sha1")
    ), "recursively put directory, using sha1 hashes to prevent useless puts" ;
						  
  ]
  in
    ("help",help commands,"help")::commands
)

  


let interactive_loop t = (
  (* log_print := Some ( fun s -> printf "%s" s ; flush stdout )  ; *)
  (* let _ = Auto_ftp.echo false in *)
  let rec r () = 
    let () = printf "" ; flush stdout ; in
    let line = read_line () in
    let line = String.strip line in
    let line = if String.starts_with line "#" then "" else line in
    let line = if String.starts_with line "!" then (
      let command = String.lchop line in
      let _ = Unix.system command in
	""
    ) else line in
    let command = String.nsplit line " " in
    let command = List.filter ( fun s -> String.strip s <> "") command in
    let () = try
	let (_,f,_) = List.find ( fun (name,f,_) -> name = List.hd command ) commands in
	  f t (List.tl command)
      with
	| Not_found -> printf "no such command ; '%s'\n" (List.hd command)
    in
      r ()
  in

    r ()
)
  
