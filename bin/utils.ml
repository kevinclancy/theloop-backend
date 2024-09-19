let string_to_yojson s : ('a, string) result =
  try Ok(Yojson.Safe.from_string s) with
  | Yojson__Common.Json_error(msg) ->
    Error(msg)

let require_length field_name ?max_length ?min_length field_val =
  if Option.is_some max_length && String.length field_val > Option.get max_length then
    Error [%string "'%{field_name}' field exceeds maximum length of %{Int.to_string @@ Option.get max_length} characters"]
  else if Option.is_some min_length && String.length field_val < Option.get min_length then
    Error [%string "'%{field_name}' field under minimum length of %{Int.to_string @@ Option.get min_length} characters"]
  else
    Ok ()

let require_uppercase_alpha field_name field_val =
  let code_A = Char.code 'A' in
  let code_Z = Char.code 'Z' in
  if String.for_all (fun x -> Char.code x >= code_A && Char.code x <= code_Z) field_val then
    Ok ()
  else
    Error [%string "'Expected value '%{field_val}' of %{field_name} to consist of uppercase alphabet characters."]