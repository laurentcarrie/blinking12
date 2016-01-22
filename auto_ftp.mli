type t

exception Connection_failed

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


val connect : host:Unix.inet_addr -> port:int -> user:string -> password:string -> t

val stat : t -> string
val cwd : t -> string -> unit
val pwd : t -> string
val list : t -> string -> file list
val mv : t -> string -> string -> unit
val rm : t -> string -> unit
val rmdir : t -> string -> unit
val mkdir : t -> string -> unit
val echo : bool -> bool
val dir_compare : t -> string -> string -> (string*diff_status) list
val get_file : t -> string -> string -> unit
val get_dir : t -> string -> string -> unit
val put_file : t -> string -> string -> unit
val put_dir : t -> string -> string -> unit
val log : ('a, unit, string, unit) format4 -> 'a
val nlst : t -> string -> string

val status : t -> string -> file option

val retrieve_password : host:string -> port:int -> user:string -> string
val cancel_password : host:string -> port:int -> user:string -> unit

val command : t -> bool -> string list -> string
