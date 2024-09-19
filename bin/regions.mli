
include Initable_intf.Initable

(** [handle_post_region country_alias state_alias body] *)
val handle_post_region : string -> string -> string -> (Dream.response, int * string) Lwt_result.t

(** [handle_get_region_alias country_alias state_alias region_alias] *)
val handle_get_region_alias : string -> string -> string -> (Dream.response, int * string) Lwt_result.t