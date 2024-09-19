open Caqti_request.Infix

let create_users =
  Caqti_type.(unit ->. unit)
  [%string
      {sql|
      CREATE TABLE users(
        id int PRIMARY KEY,
        name char(%{string_of_int Config.user_name_length}),
        biography char(%{string_of_int Config.user_biography_length}),
        time_created timestamp
      );
      |sql}
  ]

let create_countries =
  Caqti_type.(unit ->. unit)
  [%string
    {|
      CREATE TABLE countries(
        id int PRIMARY KEY,
        alias char( %{string_of_int Config.country_alias_length} ),
        name char( %{string_of_int Config.country_name_length} ),
        time_created timestamp,
        UNIQUE(alias)
      )
    |}
  ]

let create_states =
  Caqti_type.(unit ->. unit)
  [%string
      {|
      CREATE TABLE states(
        id int PRIMARY KEY,
        alias char(%{string_of_int Config.state_alias_length}),
        name char(%{string_of_int Config.state_name_length}),
        country_id int REFERENCES countries(id),
        time_created timestamp,
        UNIQUE(alias, country_id)
      )
      |}
  ]

let create_regions =
  Caqti_type.(unit ->. unit)
  [%string
      {|
        CREATE TABLE regions(
          id int PRIMARY KEY,
          alias char(%{string_of_int Config.region_alias_length}),
          name char(%{string_of_int Config.region_name_length}),
          state_id int REFERENCES states(id),
          description char(%{string_of_int Config.region_description_length}),
          time_created timestamp,
          UNIQUE(alias, state_id)
        );
      |}
  ]

let create_groups =
  Caqti_type.(unit ->. unit)
  [%string
    {|
      CREATE TABLE groups(
        id int PRIMARY KEY,
        alias char(%{string_of_int Config.group_alias_length}),
        latitude real,
        longitude real,
        region int REFERENCES regions(id),
        name char(%{string_of_int Config.group_name_length}),
        description char(%{string_of_int Config.group_description_length}),
        admin int REFERENCES users(id),
        private bool,
        time_created timestamp,
        UNIQUE(alias, region)
      );
    |}
  ]

let create_posts =
  Caqti_type.(unit ->. unit)
  [%string
    {|
      CREATE TABLE posts(
        id int PRIMARY KEY,
        group_id int REFERENCES groups(id),
        time_posted timestamp WITH TIME ZONE,
        title char(%{string_of_int Config.post_title_length}),
        body char(%{string_of_int Config.post_body_length}),
        poster int REFERENCES users(id)
      );
    |}
  ]

let create_admins =
  Caqti_type.(unit ->. unit)
  [%string
    {|
      CREATE TABLE admins(
        user_id int REFERENCES users(id),
        group_id int REFERENCES groups(id)
      );
    |}
  ]

let create_requests =
  Caqti_type.(unit ->. unit)
  [%string
    {|
      CREATE TABLE requests(
        requester_id int REFERENCES users(id),
        group_id int REFERENCES groups(id),
        message_str char(%{string_of_int Config.request_message_length}),
        PRIMARY KEY(requester_id, group_id)
      );
    |}
  ]

let create_members =
  Caqti_type.(unit ->. unit)
  [%string
      {|
        CREATE TABLE members(
          user_id int REFERENCES users(id),
          group_id int REFERENCES groups(id)
        );
      |}
  ]

let create_replies =
  Caqti_type.(unit ->. unit)
  [%string
      {|
        CREATE TABLE replies(
          id int PRIMARY KEY,
          target int REFERENCES posts(id),
          poster int REFERENCES users(id),
          body char(%{string_of_int Config.reply_body_length})
        );
      |}
  ]

let create_tables (db_conn : Caqti_lwt.connection) =
  let module DB = (val db_conn) in
  let open Lwt_result in
  let (let*) = Lwt_result.bind in
  let* () = DB.exec create_users () in
  let* () = DB.exec create_countries () in
  let* () = DB.exec create_states () in
  let* () = DB.exec create_regions () in
  let* () = DB.exec create_groups () in
  let* () = DB.exec create_posts () in
  let* () = DB.exec create_admins () in
  let* () = DB.exec create_requests () in
  let* () = DB.exec create_members () in
  let* () = DB.exec create_replies () in
  return ()

let init =
  let open Lwt_result in
  let (let*) = Lwt_result.bind in
  let* db_conn = Caqti_lwt.connect (Uri.of_string "postgresql://localhost:5432/loopdb") in
  let* () = create_tables db_conn in
  return ()

let () =
  Printf.printf "wazzup";
  match Lwt_main.run init with
  | Ok () ->
    Printf.printf "success!"
  | Error err ->
    Printf.printf "error: %s" (Caqti_error.show err)