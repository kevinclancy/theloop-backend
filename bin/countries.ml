open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Utils

let num_countries = ref 0

(** Info for registering new country *)
type post_country = {
  (* The name of the country to register *)
  name : string;
  (* Alias of the country to register *)
  alias : string;
} [@@deriving yojson]

(** Representation of a country *)
type get_country = {
  name : string;
  alias : string;
  id : int;
  time_created : string
} [@@deriving yojson]

let string_to_post_country s : (post_country, string) result =
  let (let*) = Result.bind in
  let* yojson = string_to_yojson s in
  let* post_country =
    try Ok(post_country_of_yojson yojson) with
    | Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error(exn,_) ->
      Error(Printexc.to_string_default exn)
  in
  let* () = require_length "name" ~max_length:30 post_country.name in
  let* () = require_length "alias" ~max_length:5 ~min_length:1 post_country.alias in
  let* () = require_uppercase_alpha "alias" post_country.alias in
  Ok(post_country)

let init (db_conn : Caqti_lwt.connection) =
  let open Lwt_result in
  let (let*) = Lwt_result.bind in
  let* max_country =
    map_error (fun db_err -> Caqti_error.show db_err) @@
    [%rapper
      get_one
        {sql|
        SELECT max(@int?{id}) FROM countries;
        |sql}
    ] () db_conn
  in
  num_countries := Option.value max_country ~default:0;
  return ()

let handle_post_country request_body =
  let open Lwt_result in
  let (let*) = Lwt_result.bind in
  let* post_country =
    map_error (fun msg -> (400, msg)) (lift @@ string_to_post_country request_body)
  in
  let* db_conn =
    map_error (fun db_err -> (404, Caqti_error.show db_err)) @@
    Caqti_lwt.connect (Uri.of_string "postgresql://localhost:5432/loopdb")
  in
  let* opt_alias_exists =
    map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
    [%rapper
      get_opt
      {sql|
        SELECT alias FROM countries WHERE alias = %string{new_alias}
      |sql}
    ] ~new_alias:post_country.alias db_conn
  in
  let* () =
    match opt_alias_exists with
    | Some(_) ->
      Lwt_result.fail (400, [%string "Country alias %{post_country.alias} already in use."])
    | None ->
      return ()
  in
  let time = () |> Unix.gettimeofday |> Ptime.of_float_s |> Option.get in
  let* () =
      map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
      [%rapper
        execute
          {sql|
          INSERT INTO countries (id, name, alias, time_created) VALUES (
            %int{num_countries}, %string{name}, %string{alias}, %ptime{timestamp}
          );
          |sql}
      ] ~num_countries:!num_countries ~name:post_country.name ~alias:(String.uppercase_ascii post_country.alias) ~timestamp:time db_conn;
  in
  num_countries := !num_countries + 1;
  return @@ Dream.response "Country registered"

let handle_get_country_alias alias =
  let open Lwt_result in
  let (let*) = Lwt_result.bind in
  let* db_conn =
    map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
    Caqti_lwt.connect (Uri.of_string "postgresql://localhost:5432/loopdb")
  in
  let* opt_result =
      map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
      [%rapper
        get_opt
          {sql|
          SELECT @string{name}, @string{alias}, @int{id}, @ptime{time_created} FROM countries
          WHERE alias = %string{alias}
          |sql}
      ] ~alias db_conn;
  in
  let* (name, alias, id, time_created) =
    match opt_result with
    | Some x ->
      Lwt_result.return x
    | None ->
      Lwt_result.fail (404, [%string "Country alias %{alias} not found."])
  in
  let ((year, mon, day), ((hh, mm, ss), tz_offset)) = (Ptime.to_date_time time_created) in
  let date_str = [%string "%{string_of_int mon}/%{string_of_int day}/%{string_of_int year}"] in
  let time_str = [%string "%{string_of_int hh}:%{string_of_int mm}:%{string_of_int ss}"] in
  let date_time_str = [%string "%{date_str} %{time_str} (UTC - %{string_of_int tz_offset})"] in
  let json_text =
    Yojson.Safe.to_string @@ yojson_of_get_country {
      name = String.trim name ;
      alias = String.trim alias ;
      id = id ;
      time_created = date_time_str
    }
  in
  return @@ Dream.response json_text