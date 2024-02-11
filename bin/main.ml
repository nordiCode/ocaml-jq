open Yojson.Basic.Util

let rec format_json = function
  | `Assoc pairs ->
      let formatted_pairs =
        List.map
          (fun (key, value) ->
            Printf.sprintf "  \"%s\": %s" key (format_json value))
          pairs
      in
      "{\n" ^ String.concat ",\n" formatted_pairs ^ "\n}"
  | `List values ->
      let formatted_values = List.map format_json values in
      "[\n" ^ String.concat ",\n" formatted_values ^ "\n]"
  | `Int i -> string_of_int i
  | `String s -> Printf.sprintf "\"%s\"" s
  | `Bool b -> string_of_bool b
  | `Null -> "null"
  | _ -> failwith "Unsupported JSON value"

let is_integer str =
  try
    ignore (int_of_string str);
    true
  with Failure _ -> false

let print_json_object_index index json =
  let keys = keys json in
  List.iter
    (fun key ->
      let value = json |> member key in
      Printf.printf "%s:\n" key;
      match value with
      | `List values ->
          if index < List.length values then
            Printf.printf "%s\n" (format_json (List.nth values index))
          else Printf.printf "Index out of bounds\n"
      | _ -> Printf.printf "%s\n" (format_json value))
    keys;
  flush stdout

let print_json_objects_of_key key json =
  let value = json |> member key in
  Printf.printf "%s:\n" key;
  Printf.printf "%s\n" (format_json value);
  flush stdout

let print_all_json_objects json =
  let keys = keys json in
  List.iter
    (fun key ->
      let value = json |> member key in
      Printf.printf "%s:\n" key;
      match value with
      | `List values ->
          List.iter
            (fun value -> Printf.printf "%s\n" (format_json value))
            values
      | _ -> Printf.printf "%s\n" (format_json value))
    keys;
  flush stdout

let () =
  let args = Sys.argv in
  Printf.printf "Args: %s\n" (String.concat ", " (Array.to_list args));
  match args with
  (* Case a number print that json object index *)
  | [| _; arg |]
    when String.length arg > 3
         && String.sub arg 0 2 = ".["
         && arg.[String.length arg - 1] = ']'
         && is_integer (String.sub arg 2 (String.length arg - 3)) ->
      let index_str = String.sub arg 2 (String.length arg - 3) in
      (try ignore (int_of_string index_str)
       with Failure _ ->
         Printf.printf "Invalid index: %s\n" index_str;
         exit 1);
      let json = Yojson.Basic.from_channel stdin in
      print_json_object_index (int_of_string index_str) json
  (* Case print the json objects of the arg specified key .[key] *)
  | [| _; arg |]
    when String.length arg > 3
         && String.sub arg 0 2 = ".["
         && arg.[String.length arg - 1] = ']' ->
      let key_str = String.sub arg 2 (String.length arg - 3) in
      Printf.printf "Key: %s\n" key_str;
      let json = Yojson.Basic.from_channel stdin in
      print_json_objects_of_key key_str json
  (* Case print all *)
  | [| _; "." |] ->
      let json = Yojson.Basic.from_channel stdin in
      print_all_json_objects json
  | _ -> failwith "Unsupported arguments"
