

let get_code response_channel =
  let%lwt response = Lwt_stream.to_list @@ Lwt_io.read_lines response_channel in
  (* the first line beginning with < should be "< HTTP/1.1 200 ????" *)
  let top_response_line = List.hd @@ List.filter (fun ln -> ln.[0] = '<') response in
  Lwt.return @@ List.nth (String.split_on_char ' ' top_response_line) 2

let test =
  let open Lwt_process in
  let%lwt _ = exec (shell "dropdb loopdb") in
  let%lwt _ = exec (shell "createdb loopdb") in
  let%lwt _ = exec (shell "_build/install/default/bin/init") in
  Printf.printf "Starting backend process.\n";
  let loop_process = open_process_none (shell "_build/install/default/bin/theloop") in
  let%lwt _ = Lwt_unix.sleep 0.2 in (* TODO: I need to wait until a process is listening at 8080 *)
  try%lwt
    let post_country_request =
      open_process_full
        (shell "curl http://localhost:8080/api/v1 -d \"{ name: \\\"The United States of America\\\", alias: \\\"USA\\\" }\" -v")
    in
    let%lwt response_code = get_code post_country_request#stderr in
    assert (String.equal response_code "200");

    let bad_post_country_request =
      open_process_full
        (shell "curl http://localhost:8080/api/v1 -d \"{ name: \\\"The United States of Imerica\\\" }\" -v")
    in
    let%lwt response_code = get_code bad_post_country_request#stderr in
    assert (String.equal response_code "400");

    let bad_get_country_request = open_process_full (shell "curl http://localhost:8080/api/v1/USD -v") in
    let%lwt response_code = get_code bad_get_country_request#stderr in
    assert (String.equal response_code "404");

    let good_get_country_request = open_process_full (shell "curl http://localhost:8080/api/v1/USA -v") in
    let%lwt response_code = get_code good_get_country_request#stderr in
    assert (String.equal response_code "200");

    let bad_get_state_wa_request = open_process_full (shell "curl http://localhost:8080/api/v1/USA/WA -v") in
    let%lwt response_code = get_code bad_get_state_wa_request#stderr in
    assert (String.equal response_code "404");

    let good_post_state_wa_request =
      open_process_full
        (shell "curl http://localhost:8080/api/v1/USA -d \"{ name: \\\"Washington\\\", alias: \\\"WA\\\" }\" -v")
    in
    let%lwt response_code = get_code good_post_state_wa_request#stderr in
    assert (String.equal response_code "200");

    let good_get_state_wa_request = open_process_full (shell "curl http://localhost:8080/api/v1/USA/WA -v") in
    let%lwt response_code = get_code good_get_state_wa_request#stderr in
    assert (String.equal response_code "200");

    let bad_post_state_id_request_1 =
      open_process_full
        (shell "curl http://localhost:8080/api/v1/USA -d \"{ alias: \\\"ID\\\" }\" -v")
    in
    let%lwt response_code = get_code bad_post_state_id_request_1#stderr in
    assert (String.equal response_code "400");

    let bad_post_state_id_request_2 =
      open_process_full
        (shell "curl http://localhost:8080/api/v1/USD -d \"{ name: \\\"Idaho\\\", alias: \\\"ID\\\" }\" -v")
    in
    let%lwt response_code = get_code bad_post_state_id_request_2#stderr in
    assert (String.equal response_code "404");

    let good_post_state_id_request =
      open_process_full
        (shell "curl http://localhost:8080/api/v1/USA -d \"{ name: \\\"Idaho\\\", alias: \\\"ID\\\" }\" -v")
    in
    let%lwt response_code = get_code good_post_state_id_request#stderr in
    assert (String.equal response_code "200");

    let good_post_region_nolym_request =
      open_process_full
        (shell "curl http://localhost:8080/api/v1/USA/WA -d \"{ name: \\\"North Olympic Peninsula\\\", alias: \\\"NOLYM\\\", description: \\\"The northern part of the Olympic Peninsula, including Port Angeles, Forks, Sequim, and Port Townsend.\\\" }\" -v")
    in
    let%lwt response_code = get_code good_post_region_nolym_request#stderr in
    assert (String.equal response_code "200");

    Printf.printf "Terminating backend process.\n";
    loop_process#terminate;
    Lwt.return ()
  with
  | exn ->
    Printf.printf "Terminating backend process.\n";
    loop_process#terminate;
    raise exn

let () =
  Lwt_main.run test;
  Printf.printf "Test succeeded!\n"


