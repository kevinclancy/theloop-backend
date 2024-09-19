
include Initable_intf.Initable

val handle_post_country : string -> (Dream.response, int * string) Lwt_result.t

val handle_get_country_alias : string -> (Dream.response, int * string) Lwt_result.t