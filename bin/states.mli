
include Initable_intf.Initable

(** [handle_post_state country_alias body] posts a state under the country [country_alias]
    [body] is a json table containing the following fields

    * name: string - the name of the state

    * alias: string - the alias of the state, between 1 and 5 alphabetic capital letters

    *)
val handle_post_state : string -> string -> (Dream.response, int * string) Lwt_result.t

val handle_get_state_alias : string -> string -> (Dream.response, int * string) Lwt_result.t