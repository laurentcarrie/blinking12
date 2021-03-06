open Printf
open ExtList
open ExtString

let log = Auto_ftp.log

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
  printf "help\tprint this help\n" ;
  printf "!command\tlocally runs [command]\n" ;
  List.iter ( fun (name,_,h) ->
    printf "%s\t\t%s\n" name h
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

    "getdir",(fun t args ->
      let () = match args with
	| dirname::[] -> let _ = Auto_ftp.get_dir t dirname dirname in ()
	| distant_dirname::local_dirname::[] -> let _ = Auto_ftp.get_dir t distant_dirname local_dirname in ()
	| _ -> (log "%s" "bad args for get_dir")
      in  ()
    ),"get_dir [dirname | distant_dirname local_dirname] : recursively gets a directory, using sha1 stored to optimize" ;

    "putdir_no_sha1",(fun t args ->
      match args with
	| dirname::[] ->  let _ = Auto_ftp.put_dir ~use_sha1:false t dirname dirname in ()
	| local_dirname::distant_dirname::[] -> let _ = Auto_ftp.put_dir ~use_sha1:false t local_dirname distant_dirname in ()
	| _ -> (log "%s" "bad args for put_dir_no_sha1")
    ), "recursively put directory, ignoring sha1 hashes (really puts all files)" ;
						  
    "putdir",(fun t args ->
      match args with
	| dirname::[] ->  let _ = Auto_ftp.put_dir ~use_sha1:true t dirname dirname in ()
	| local_dirname::distant_dirname::[] -> let _ = Auto_ftp.put_dir ~use_sha1:true t local_dirname distant_dirname in ()
	| _ -> (log "%s" "bad args for put_dir_no_sha1")
    ), "recursively put directory, using sha1 hashes to prevent useless puts" ;


    "put",(fun t args -> match args with
      | filename::[] -> let _ = Auto_ftp.put_file ~use_sha1:true t filename filename in ()
      | local::distant::[] ->  let _ = Auto_ftp.put_file ~use_sha1:true t local distant in ()
      | _ -> (log "%s" "bad args for put")
    ),"put file, using sha1 hash to prevent useless write" ;
				  
    "put_no_sha1",(fun t args -> match args with
      | filename::[] -> let _ = Auto_ftp.put_file ~use_sha1:false t filename filename in ()
      | local::distant::[] ->  let _ = Auto_ftp.put_file ~use_sha1:false t local distant in ()
      | _ -> (log "%s" "bad args for put_no_sha1")
    ),"put file, ignoring sha1 hash (always write file)" ;
				  
    "mv",(fun t args -> match args with
      | old_name::new_name::[] ->  let _ = Auto_ftp.mv t old_name new_name in ()
      | _ -> (log "%s" "bad args for mv")
    ),"mv old_name new_name : unlinks new_name if necessary, move old_name to new_name" ;
				  
    "rm",(fun t args -> match args with
      | name::[] ->  let _ = Auto_ftp.rm t name in ()
      | _ -> (log "%s" "bad args for rm")
    ),"rm name : unlinks file name" ;
				  
    "getdir",(fun t args -> match args with
      | name::[] ->  let _ = Auto_ftp.get_dir t name in ()
      | distant_name::local_name::[] ->  let _ = Auto_ftp.get_dir t distant_name local_name in ()
      | _ -> (log "%s" "bad args for getdir")
    ),"getdir distant_name [local_name] : gets directory" ;
				  
    "rmdir",(fun t args -> match args with
      | name::[] ->  let _ = Auto_ftp.rmdir t name in ()
      | _ -> (log "%s" "bad args for rmdir")
    ),"rmdir dirname : recursively removes  remote dirname" ;
				  
    "mkdir",(fun t args -> match args with
      | name::[] ->  let _ = Auto_ftp.mkdir t name in ()
      | _ -> (log "%s" "bad args for rmdir")
    ),"mkdir dirname : creates remote dirname" ;
				  
    "cd",(fun t args -> match args with
      | name::[] ->  let _ = Auto_ftp.command t false ["CWD";name] in ()
      | _ -> (log "%s" "bad args for cd")
    ),"cd dirname : change remote directory to dirname" ;
				  
    "pwd",(fun t args -> match args with
      | name::[] ->  let pwd = Auto_ftp.pwd t in printf "%s\n" pwd ; ()
      | _ -> (log "%s" "bad args for pwd")
    ),"pwd : print remote current directory" ;
				  
  ]
  in
  let commands = List.sort ~cmp:(fun (n1,_,_) (n2,_,_) -> String.compare n1 n2) commands in
    ("help",help commands,"help")::commands
)

  


let interactive_loop t read = (
  (* log_print := Some ( fun s -> printf "%s" s ; flush stdout )  ; *)
  (* let _ = Auto_ftp.echo false in *)
  let rec r () = 
    let () = printf "" ; flush stdout ; in
    let line = read () in
    let line = String.strip line in
    let line = if String.starts_with line "#" then "" else line in
    let line = if String.starts_with line "!" then (
      let command = String.lchop line in
      let _ = Unix.system command in
	""
    ) else line in
    let command = String.nsplit line " " in
    let command = List.filter ( fun s -> String.strip s <> "") command in
    let () = match command with
      | [] -> ()
      | hd::args -> (
	  try
	    let (_,f,_) = List.find ( fun (name,f,_) -> name = hd ) commands in
	      f t args
	  with
	    | List.Empty_list -> printf "empty list\n"
	    | Not_found -> printf "no such command ; '%s'\n" (List.hd command)
	) 
    in
      r ()
  in

    r ()
)
  
