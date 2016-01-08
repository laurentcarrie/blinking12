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
      log "%s %s\n" ( if f.Auto_ftp.is_directory then "d" else " ") f.Auto_ftp.name 
    ) data ;
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
      log "%s %s\n" (string_of_status status) name
    ) data ;
)

let interactive_loop t = (
  (* log_print := Some ( fun s -> printf "%s" s ; flush stdout )  ; *)
  let () = Auto_ftp.echo true in
  let rec r () = 
    let () = printf ">" ; flush stdout ; in
    let line = read_line () in
    let line = String.strip line in
    let line = if String.starts_with line "#" then "" else line in
    let () = match (String.nsplit line " ")  with
      | ["list"] 
      | ["ls"] -> print_list t "."
      | ["list";d] 
      | ["ls";d] -> print_list t d
(*
      | ["ls";d] -> let _ = list t "" in ()
      | ["list";d] -> let _ = list t "" in ()
*)
      | ["help"] -> help () 
      | ["pwd"] -> let ret = Auto_ftp.pwd t in printf ">>%s\n" ret ; ()
      | ["cd";d] -> let _ = Auto_ftp.command t false ["CWD";d] in ()
      | ["cwd";d] -> let _ = Auto_ftp.command t false ["CWD";d] in ()
      | ["get";filename] -> let _ = Auto_ftp.get_file t filename filename in ()
      | ["get";distant_filename;local_filename] -> let _ = Auto_ftp.get_file t distant_filename local_filename in ()
      | ["put";filename] -> let _ = Auto_ftp.put_file t filename filename in ()
      | ["put";local_filename;distant_filename] -> let _ = Auto_ftp.put_file t local_filename distant_filename in ()
      | ["mv";old_name;new_name] -> let _ = Auto_ftp.mv t old_name new_name in ()
      | ["rm";name] -> let _ = Auto_ftp.rm t name  in ()
      | ["rmdir";name] -> let _ = Auto_ftp.rmdir t name  in ()
      | ["mkdir";name] -> let _ = Auto_ftp.mkdir t name  in ()
      | ["echo";"on"] -> Auto_ftp.echo true 
      | ["echo";"off"] -> Auto_ftp.echo false
      | ["compare";local;distant] -> let _ = print_compare t local distant in ()
      (* | l ->  let _ = command t false l  in () *)
      | [] -> ()
      | s -> log "-> unknown command '%s'\n" line ; flush stdout ; 
    in
      r ()
  in
    r()
)
  
