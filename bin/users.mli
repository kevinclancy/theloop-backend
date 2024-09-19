
include Initable_intf.Initable

val handle_post_user : string -> (Dream.response, int * string) Lwt_result.t

val handle_get_user_n : int -> (Dream.response, int * string) Lwt_result.t