type t

type file = {
  name : string ;
  access : string ;
  group : string ;
  is_directory : bool ;
}

val connect : host:Unix.inet_addr -> port:int -> user:string -> password:string -> t

val loop : t -> unit

val cwd : t -> string -> unit
val pwd : t -> string
val list : t -> file list
