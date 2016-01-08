type t

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

val cwd : t -> string -> unit
val pwd : t -> string
val list : t -> string -> file list
val mv : t -> string -> string -> unit
val rm : t -> string -> unit
val rmdir : t -> string -> unit
val mkdir : t -> string -> unit
val echo : bool -> unit
val dir_compare : t -> string -> string -> (string*diff_status) list
val get_file : t -> string -> string -> unit
val put_file : t -> string -> string -> unit
val log : ('a, unit, string, unit) format4 -> 'a

val command : t -> bool -> string list -> string
