open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Utils

(** Info for registering new user *)
type post_user = {
  (* The name of the user to register *)
  name : string;
  (* A paragraph that describes the user and their interests *)
  biography : string
} [@@deriving yojson]

(** Description of an user queried *)
type get_user = {
  name : string;
  biography : string ;
  time_created : string
} [@@deriving yojson]

let num_users = ref 0

let init (db_conn : Caqti_lwt.connection) =
  let open Lwt_result in
  let (let*) = Lwt_result.bind in
  let* max_user =
    map_error (fun db_err -> Caqti_error.show db_err) @@
    [%rapper
      get_one
        {sql|
        SELECT max(@int?{id}) FROM users;
        |sql}
    ] () db_conn;
  in
  num_users := Option.value max_user ~default:0;
  return ()

let string_to_post_user s : (post_user, string) result =
  let (let*) = Result.bind in
  let* yojson = string_to_yojson s in
  let* post_user =
    try Ok(post_user_of_yojson yojson) with
    | Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error(exn,_) ->
      Error(Printexc.to_string_default exn)
  in
  let* () = require_length "name" ~max_length:80 post_user.name in
  let* () = require_length "biography" ~max_length:140 post_user.biography in
  Ok(post_user)

let handle_post_user request_body =
  let open Lwt_result in
  let (let*) = Lwt_result.bind in
  let* post_user =
    map_error
      (fun msg -> (400, msg))
      (lift @@ string_to_post_user request_body)
  in
  let* db_conn =
    map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
    Caqti_lwt.connect (Uri.of_string "postgresql://localhost:5432/loopdb")
  in
  let time = () |> Unix.gettimeofday |> Ptime.of_float_s |> Option.get in
  let* () =
      map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
      [%rapper
        execute
          {sql|
          INSERT INTO users (id, name, biography, time_created) VALUES (
            %int{num_users},
            %string{name},
            %string{bio},
            %ptime{timestamp}
          );
          |sql}
      ] ~num_users:!num_users ~name:post_user.name ~bio:post_user.biography ~timestamp:time db_conn;
  in
  num_users := !num_users + 1;
  return @@ Dream.response "User registered"

let handle_get_user_n n =
  let open Lwt_result in
  let (let*) = Lwt_result.bind in
  let* db_conn =
    map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
    Caqti_lwt.connect (Uri.of_string "postgresql://localhost:5432/loopdb")
  in
  let* (name, biography, time_created) =
      map_error (fun db_err -> (400, Caqti_error.show db_err)) @@
      [%rapper
        get_one
          {sql|
          SELECT @string{name}, @string{biography}, @ptime{time_created}, id FROM users
          WHERE id = %int{n}
          |sql}
      ] ~n db_conn;
  in
  let ((year, mon, day), ((hh, mm, ss), tz_offset)) = (Ptime.to_date_time time_created) in
  let date_str = [%string "%{string_of_int mon}/%{string_of_int day}/%{string_of_int year}"] in
  let time_str = [%string "%{string_of_int hh}:%{string_of_int mm}:%{string_of_int ss}"] in
  let date_time_str = [%string "%{date_str} %{time_str} (UTC - %{string_of_int tz_offset})"] in
  let json_text =
    Yojson.Safe.to_string @@ yojson_of_get_user {
      name = String.trim name ;
      biography = String.trim biography ;
      time_created = date_time_str
    }
  in
  return @@ Dream.response json_text