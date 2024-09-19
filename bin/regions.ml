open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Utils

(** Info for registering new region *)
type post_region = {
  (* The name of the region to register *)
  name : string;
  (* A description of the region *)
  description : string ;
  (* Alias of the region, a state-unique identifier appearing in URLs *)
  alias : string;
} [@@deriving yojson]

(** Description of a state/province returned from query *)
type get_region = {
  name : string;
  alias : string ;
  description : string ;
  time_created : string
} [@@deriving yojson]

let num_regions = ref 0

let init (db_conn : Caqti_lwt.connection) =
  let open Lwt_result in
  let (let*) = Lwt_result.bind in
  let* max_region =
    map_error (fun db_err -> Caqti_error.show db_err) @@
    [%rapper
      get_one
        {sql|
        SELECT max(@int?{id}) FROM regions;
        |sql}
    ] () db_conn
  in
  num_regions := Option.value max_region ~default:0;
  return ()

let string_to_post_region s : (post_region, string) result =
  let (let*) = Result.bind in
  let* yojson = string_to_yojson s in
  let* post_region =
    try Ok(post_region_of_yojson yojson) with
    | Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error(exn,_) ->
      Error(Printexc.to_string_default exn)
  in
  let* () = require_length "name" ~max_length:Config.region_name_length post_region.name in
  let* () = require_length "alias" ~max_length:Config.region_alias_length ~min_length:1 post_region.alias in
  Ok(post_region)

let handle_post_region country_alias state_alias request_body =
  let open Lwt_result in
  let (let*) = Lwt_result.bind in
  let state_alias = String.init 5 (fun n -> if n < String.length state_alias then state_alias.[n] else ' ') in
  let* post_region =
    map_error (fun msg -> (400, msg)) (lift @@ string_to_post_region request_body)
  in
  let* db_conn =
    map_error (fun db_err -> (404, Caqti_error.show db_err)) @@
    Caqti_lwt.connect (Uri.of_string "postgresql://localhost:5432/loopdb")
  in
  let* opt_country_id =
    map_error (fun db_err -> (404, Caqti_error.show db_err)) @@
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
    | Some x ->
      Lwt_result.return x
    | None ->
      Lwt_result.fail (404, [%string "Country alias %{String.trim country_alias} not found."])
  in
  let* opt_state_id =
    map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
    [%rapper
      get_opt
      {sql|
        SELECT @int{states.id} FROM states
        WHERE alias = %string{state_alias} AND id = %int{country_id}
      |sql}
    ] ~state_alias ~country_id db_conn
  in
  let* state_id =
    match opt_state_id with
    | Some(x) ->
      return x
    | None ->
      Lwt_result.fail (404, [%string "State alias %{String.trim state_alias} not found under %{String.trim country_alias}."])
  in
  let* opt_alias_exists =
    map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
    [%rapper
      get_opt
      {sql|
        SELECT alias FROM regions
        WHERE alias = %string{new_alias} AND state_id = %int{state_id}
      |sql}
    ] ~new_alias:post_region.alias ~state_id db_conn
  in
  let* () =
    match opt_alias_exists with
    | Some(_) ->
      Lwt_result.fail (400, [%string "Region alias %{post_region.alias} already in use for state %{String.trim state_alias}."])
    | None ->
      return ()
  in
  let time = () |> Unix.gettimeofday |> Ptime.of_float_s |> Option.get in
  let* () =
      map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
      [%rapper
        execute
          {sql|
          INSERT INTO regions (id, name, alias, description, time_created, state_id) VALUES (
            %int{num_regions}, %string{name}, %string{alias}, %string{description}, %ptime{timestamp}, %int{state_id}
          );
          |sql}
      ] ~num_regions:!num_regions ~name:post_region.name ~alias:(String.uppercase_ascii post_region.alias)
        ~description:post_region.description ~timestamp:time ~state_id db_conn;
  in
  num_regions := !num_regions + 1;
  return @@ Dream.response "Region registered"

let handle_get_region_alias country_alias state_alias region_alias =
  let open Lwt_result in
  let (let*) = Lwt_result.bind in
  let* db_conn =
    map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
    Caqti_lwt.connect (Uri.of_string "postgresql://localhost:5432/loopdb")
  in
  (* TODO: respond with an informative error when the state_alias is not found *)
  let* (name, alias, description, time_created) =
      map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
      [%rapper
        get_one
          {sql|
          SELECT @string{regions.name}, @string{regions.alias}, @string{regions.description}, @ptime{states.time_created}
          FROM countries, states, regions
          WHERE regions.alias = %string{region_alias} AND
                regions.state_id = states.id AND
                states.alias = %string{state_alias} AND
                states.country_id = countries.id AND
                countries.alias = %string{country_alias}
          |sql}
      ] ~region_alias ~state_alias ~country_alias db_conn;
  in
  let ((year, mon, day), ((hh, mm, ss), tz_offset)) = (Ptime.to_date_time time_created) in
  let date_str = [%string "%{string_of_int mon}/%{string_of_int day}/%{string_of_int year}"] in
  let time_str = [%string "%{string_of_int hh}:%{string_of_int mm}:%{string_of_int ss}"] in
  let date_time_str = [%string "%{date_str} %{time_str} (UTC - %{string_of_int tz_offset})"] in
  let json_text =
    Yojson.Safe.to_string @@ yojson_of_get_region {
      name = String.trim name ;
      alias = String.trim alias ;
      description = String.trim description ;
      time_created = date_time_str
    }
  in
  return @@ Dream.response json_text