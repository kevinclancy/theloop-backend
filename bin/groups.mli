
include Initable_intf.Initable

(** [handle_post_group country_alias state_alias region_alias body] *)
val handle_post_group : string -> string -> string -> string -> (Dream.response, int * string) Lwt_result.t

(** [handle_get_group_alias country_alias state_alias region_alias group_alias] *)
val handle_get_group_alias : string -> string -> string -> string -> (Dream.response, int * string) Lwt_result.t