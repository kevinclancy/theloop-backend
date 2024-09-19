open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Utils

(**
  http post request data for group
*)
type post_group = {
  (* The name of the group to register *)
  name : string;
  (* A description of the group *)
  description : string ;
  (** The latitude of a position near where this group typically meets *)
  latitude : float ;
  (** The longitude of a position near where this group typically meets *)
  longitude : float ;
  (** Alias of the group, a region-unique identifier appearing in URLs *)
  alias : string;
  (** True iff this group is private *)
  is_private : bool
} [@@deriving yojson]

(** http GET response data for group *)
type get_group = {
  (* TODO: add id as a field to this and all other GET responses *)
  name : string ;
  alias : string ;
  description : string ;
  (** The latitude of a position near where this group typically meets *)
  latitude : float ;
  (** The longitude of a position near where this group typically meets *)
  longitude : float ;
  time_created : string
} [@@deriving yojson]

let num_groups = ref 0

let string_to_post_group s : (post_group, string) result =
  let (let*) = Result.bind in
  let* yojson = string_to_yojson s in
  let* post_group =
    try Ok(post_group_of_yojson yojson) with
    | Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error(exn,_) ->
      Error(Printexc.to_string_default exn)
  in
  let* () = require_length "name" ~max_length:Config.group_name_length post_group.name in
  let* () = require_length "alias" ~max_length:Config.group_alias_length ~min_length:1 post_group.alias in
  let* () = require_length "description" ~max_length:Config.group_description_length ~min_length:1 post_group.alias in
  let* () = require_uppercase_alpha "alias" post_group.alias in
  Ok(post_group)

let init (db_conn : Caqti_lwt.connection) =
  let open Lwt_result in
  let (let*) = Lwt_result.bind in
  let* max_group =
    map_error (fun db_err -> Caqti_error.show db_err) @@
    [%rapper
      get_one
        {sql|
        SELECT max(@int?{id}) FROM groups;
        |sql}
    ] () db_conn
  in
  num_groups := Option.value max_group ~default:0;
  return ();;

let handle_post_group country_alias state_alias region_alias request_body =
  let open Lwt_result in
  let (let*) = Lwt_result.bind in
  let* post_group =
    map_error (fun msg -> (400, msg)) (lift @@ string_to_post_group request_body)
  in
  let* db_conn =
    map_error (fun db_err -> (404, Caqti_error.show db_err)) @@
    Caqti_lwt.connect (Uri.of_string "postgresql://localhost:5432/loopdb")
  in
  let* opt_region_id =
    map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
    [%rapper
      get_opt
      {sql|
        SELECT @int{regions.id} FROM countries,states,regions
        WHERE countries.alias = %string{country_alias} AND
              states.country_id = countries.id AND
              states.alias = %string{state_alias} AND
              regions.state_id = states.id AND
              regions.alias = %string{region_alias}
      |sql}
    ] ~country_alias ~state_alias ~region_alias db_conn
  in
  let* region_id =
    match opt_region_id with
    | Some(x) ->
      return x
    | None ->
      Lwt_result.fail (404, [%string "Region %{country_alias}/%{state_alias}/%{region_alias} not found."])
  in
  let* opt_alias_exists =
    map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
    [%rapper
      get_opt
      {sql|
        SELECT groups.alias FROM regions,groups
        WHERE groups.region_id = %int{region_id} AND
              alias = %string{new_alias}
      |sql}
    ] ~new_alias:post_group.alias ~region_id db_conn
  in
  let* () =
    match opt_alias_exists with
    | Some(_) ->
      Lwt_result.fail (400, [%string "Group alias %{post_group.alias} already in under region %{region_alias}."])
    | None ->
      return ()
  in
  let time = () |> Unix.gettimeofday |> Ptime.of_float_s |> Option.get in
  let* () =
      map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
      [%rapper
        execute
          {sql|
          INSERT INTO groups (id, name, alias, region, latitude, longitude, description, admin, private, time_created) VALUES (
            %int{num_groups}, %string{name}, %string{alias}, %int{region_id},
            %float{latitude}, %float{longitude}, %string{description}, %int{admin}, %bool{is_private}, %ptime{timestamp}
          );
          |sql}
      ] ~num_groups:!num_groups ~name:post_group.name ~alias:(String.uppercase_ascii post_group.alias) ~region_id
        ~latitude:post_group.latitude ~longitude:post_group.longitude ~description:post_group.description
        ~admin:0 ~is_private:false ~timestamp:time db_conn;
  in
  num_groups := !num_groups + 1;
  return @@ Dream.response "Country registered"

let handle_get_group_alias country_alias state_alias region_alias group_alias =
  let open Lwt_result in
  let (let*) = Lwt_result.bind in
  let* db_conn =
    map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
    Caqti_lwt.connect (Uri.of_string "postgresql://localhost:5432/loopdb")
  in
  (* TODO: respond with an informative error when the state_alias is not found *)
  let* (name, alias, description, latitude, longitude, time_created) =
      map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
      [%rapper
        get_one
          {sql|
          SELECT @string{groups.name}, @string{groups.alias}, @string{groups.description}, @float{groups.latitude}, @float{groups.longitude}, @ptime{states.time_created}
          FROM countries, states, regions, groups
          WHERE groups.alias = %string{group_alias} AND
                groups.region_id = regions.id AND
                regions.alias = %string{region_alias} AND
                regions.state_id = states.id AND
                states.alias = %string{state_alias} AND
                states.country_id = countries.id AND
                countries.alias = %string{country_alias}
          |sql}
      ] ~group_alias ~region_alias ~state_alias ~country_alias db_conn;
  in
  let ((year, mon, day), ((hh, mm, ss), tz_offset)) = (Ptime.to_date_time time_created) in
  let date_str = [%string "%{string_of_int mon}/%{string_of_int day}/%{string_of_int year}"] in
  let time_str = [%string "%{string_of_int hh}:%{string_of_int mm}:%{string_of_int ss}"] in
  let date_time_str = [%string "%{date_str} %{time_str} (UTC - %{string_of_int tz_offset})"] in
  let json_text =
    Yojson.Safe.to_string @@ yojson_of_get_group {
      name = String.trim name ;
      alias = String.trim alias ;
      latitude ;
      longitude ;
      description = String.trim description ;
      time_created = date_time_str
    }
  in
  return @@ Dream.response json_text
