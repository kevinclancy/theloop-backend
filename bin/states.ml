open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Utils

(** Info for registering new country *)
type post_state = {
  (* The name of the state to register *)
  name : string;
  (* Alias of the state to register *)
  alias : string;
} [@@deriving yojson]

(** Description of a state/province returned from query *)
type get_state = {
  name : string;
  alias : string ;
  time_created : string
} [@@deriving yojson]

let num_states = ref 0

let init (db_conn : Caqti_lwt.connection) =
  let open Lwt_result in
  let (let*) = Lwt_result.bind in
  let* max_state =
    map_error (fun db_err -> Caqti_error.show db_err) @@
    [%rapper
      get_one
        {sql|
        SELECT max(@int?{id}) FROM states;
        |sql}
    ] () db_conn
  in
  num_states := Option.value max_state ~default:0;
  return ()

let string_to_post_state s : (post_state, string) result =
  let (let*) = Result.bind in
  let* yojson = string_to_yojson s in
  let* post_state =
    try Ok(post_state_of_yojson yojson) with
    | Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error(exn,_) ->
      Error(Printexc.to_string_default exn)
  in
  let* () = require_length "name" ~max_length:30 post_state.name in
  let* () = require_length "alias" ~max_length:5 ~min_length:1 post_state.alias in
  let* () = require_uppercase_alpha "alias" post_state.alias in
  Ok(post_state)

let handle_post_state country_alias request_body =
  let open Lwt_result in
  let (let*) = Lwt_result.bind in
  let country_alias = String.init 5 (fun n -> if n < String.length country_alias then country_alias.[n] else ' ') in
  let* post_state =
    map_error (fun msg -> (400, msg)) (lift @@ string_to_post_state request_body)
  in
  let* db_conn =
    map_error (fun db_err -> (404, Caqti_error.show db_err)) @@
    Caqti_lwt.connect (Uri.of_string "postgresql://localhost:5432/loopdb")
  in
  let* opt_country_id =
    map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
    [%rapper
      get_opt
      {sql|
        SELECT @int{id} FROM countries
        WHERE alias = %string{country_alias}
      |sql}
    ] ~country_alias db_conn
  in
  let* country_id =
    match opt_country_id with
    | Some(x) ->
      return x
    | None ->
      Lwt_result.fail (404, [%string "Country alias %{String.trim country_alias} not found."])
  in
  let* opt_alias_exists =
    map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
    [%rapper
      get_opt
      {sql|
        SELECT states.alias FROM states JOIN countries ON states.country_id = countries.id
        WHERE states.alias = %string{new_alias} AND countries.alias = %string{parent_country_alias}
      |sql}
    ] ~new_alias:post_state.alias ~parent_country_alias:country_alias db_conn
  in
  let* () =
    match opt_alias_exists with
    | Some(_) ->
      Lwt_result.fail (400, [%string "State alias %{post_state.alias} already in use for country %{String.trim country_alias}."])
    | None ->
      return ()
  in
  let time = () |> Unix.gettimeofday |> Ptime.of_float_s |> Option.get in
  let* () =
      map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
      [%rapper
        execute
          {sql|
          INSERT INTO states (id, name, alias, time_created, country_id) VALUES (
            %int{num_states}, %string{name}, %string{alias}, %ptime{timestamp}, %int{country_id}
          );
          |sql}
      ] ~num_states:!num_states ~name:post_state.name ~alias:(String.uppercase_ascii post_state.alias)
        ~timestamp:time ~country_id:country_id db_conn;
  in
  num_states := !num_states + 1;
  return @@ Dream.response "State registered"

let handle_get_state_alias country_alias state_alias =
  let open Lwt_result in
  let (let*) = Lwt_result.bind in
  let* db_conn =
    map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
    Caqti_lwt.connect (Uri.of_string "postgresql://localhost:5432/loopdb")
  in
  let* opt_country_id =
    map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
    [%rapper
      get_opt
        {sql|
        SELECT @int{id} FROM countries
        WHERE alias = %string{country_alias}
        |sql}
    ] ~country_alias db_conn;
  in
  let* country_id =
    match opt_country_id with
    | Some x ->
      Lwt_result.return x
    | None ->
      Lwt_result.fail (404, [%string "Country alias %{country_alias} not found."])
  in
  (* TODO: respond with an informative error when the country_alias is not found *)
  let* opt_state_fields =
      map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
      [%rapper
        get_opt
          {sql|
          SELECT @string{name}, @string{alias}, @ptime{time_created}
          FROM states
          WHERE country_id = %int{country_id} AND alias = %string{state_alias}
          |sql}
      ] ~country_id ~state_alias db_conn;
  in
  let* (name, alias, time_created) =
    match opt_state_fields with
    | Some x ->
      Lwt_result.return x
    | None ->
      Lwt_result.fail (404, [%string "State alias %{state_alias} not found under country %{country_alias}."])
  in
  let ((year, mon, day), ((hh, mm, ss), tz_offset)) = (Ptime.to_date_time time_created) in
  let date_str = [%string "%{string_of_int mon}/%{string_of_int day}/%{string_of_int year}"] in
  let time_str = [%string "%{string_of_int hh}:%{string_of_int mm}:%{string_of_int ss}"] in
  let date_time_str = [%string "%{date_str} %{time_str} (UTC - %{string_of_int tz_offset})"] in
  let json_text =
    Yojson.Safe.to_string @@ yojson_of_get_state {
      name = String.trim name ;
      alias = String.trim alias ;
      time_created = date_time_str
    }
  in
  return @@ Dream.response json_text