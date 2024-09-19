let api_prefix = "api/v1"

(** Total number of users registered *)

(*
type post_country = {
  name : string;
  alias : string;
} *)

(* let bleh =
  [%rapper
    execute
      {sql|
      CREATE TABLE users (
        id int primary key,
        name char[80],
        bio char[200],
        time_created timestamp
      );
      |sql}
  ] *)

  (**
      Returns n, where

      - n is the number of users in the database
  *)
let load_ids () =
  let open Lwt_result in
  let (let*) = Lwt_result.bind in
  let (and*) = Lwt_result.both in
  let* db_conn =
    map_error (fun db_err -> Caqti_error.show db_err) @@
    Caqti_lwt.connect (Uri.of_string "postgresql://localhost:5432/loopdb")
  in
  let* max_user =
      map_error (fun db_err -> Caqti_error.show db_err) @@
      [%rapper
        get_one
          {sql|
          SELECT max(@int?{id}) FROM users;
          |sql}
      ] () db_conn;
  and* max_country =
    map_error (fun db_err -> Caqti_error.show db_err) @@
    [%rapper
      get_one
        {sql|
        SELECT max(@int?{id}) FROM countries;
        |sql}
    ] () db_conn;
  in
  return (Option.value max_user ~default:0, Option.value max_country ~default:0)

let init () =
  let open Lwt_result in
  let (let*) = Lwt_result.bind in
  let* db_conn =
    map_error (fun db_err -> Caqti_error.show db_err) @@
    Caqti_lwt.connect (Uri.of_string "postgresql://localhost:5432/loopdb")
  in
  let* () = Users.init db_conn in
  let* () = Countries.init db_conn in
  return ()

let () =
  ignore load_ids;
  ignore Groups.init;
  let () =
    match Lwt_main.run @@ init () with
    | Ok () ->
      ()
    | Error msg ->
      Printf.printf "%s" msg;
      exit 0
  in
  ignore @@ Lwt_unix.on_signal Sys.sigterm (fun _ -> failwith "Caught SIGTERM");
  Dream.run
  @@ Dream.logger
  @@ Dream.router [

  Dream.post [%string "%{api_prefix}/users"] (fun request ->
    let (let*) = Lwt.bind in
    let* body = Dream.body request in
    let* result = Users.handle_post_user body in
    Lwt.return (
      match result with
      | Ok response ->
        response
      | Error (code, msg) ->
        Dream.response ~code msg
    );
  );

  Dream.get [%string "%{api_prefix}/users/:n"] (fun request ->
    let (let*) = Lwt.bind in
    let n = int_of_string (Dream.param request "n") in
    let* result = Users.handle_get_user_n n in
    Lwt.return (
      match result with
      | Ok response ->
        response
      | Error (code, msg) ->
        Dream.response ~code msg
    )
  );

  Dream.post [%string "%{api_prefix}"] (fun request ->
    let (let*) = Lwt.bind in
    let* body = Dream.body request in
    let* result = Countries.handle_post_country body in
    Lwt.return (
      match result with
      | Ok response ->
        response
      | Error (code, msg) ->
        Dream.response ~code msg
    );
  );

  Dream.get [%string "%{api_prefix}/:alias"] (fun request ->
    let (let*) = Lwt.bind in
    let alias = Dream.param request "alias" in
    let* result = Countries.handle_get_country_alias alias in
    Lwt.return (
      match result with
      | Ok response ->
        response
      | Error (code, msg) ->
        Dream.response ~code msg
    )
  );

  Dream.post [%string "%{api_prefix}/:country_alias"] (fun request ->
    let (let*) = Lwt.bind in
    let* body = Dream.body request in
    let country_alias = Dream.param request "country_alias" in
    let* result = States.handle_post_state country_alias body in
    Lwt.return (
      match result with
      | Ok response ->
        response
      | Error (code, msg) ->
        Dream.response ~code msg
    );
  );

  Dream.get [%string "%{api_prefix}/:country_alias/:state_alias"] (fun request ->
    let (let*) = Lwt.bind in
    let country_alias = Dream.param request "country_alias" in
    let state_alias = Dream.param request "state_alias" in
    let* result = States.handle_get_state_alias country_alias state_alias in
    Lwt.return (
      match result with
      | Ok response ->
        response
      | Error (code, msg) ->
        Dream.response ~code msg
    );
  );

  Dream.post [%string "%{api_prefix}/:country_alias/:state_alias"] (fun request ->
    let (let*) = Lwt.bind in
    let country_alias = Dream.param request "country_alias" in
    let state_alias = Dream.param request "state_alias" in
    let* body = Dream.body request in
    let* result = Regions.handle_post_region country_alias state_alias body in
    Lwt.return (
      match result with
      | Ok response ->
        response
      | Error (code, msg) ->
        Dream.response ~code msg
    );
  );

  Dream.get "/echo/:word"
    (fun request ->
      Dream.html (Dream.param request "word"));
  ]
