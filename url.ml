(* approx: proxy server for Debian archive files
   Copyright (C) 2017  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Config
open Log
open Util

let string_of_time t =
  Netdate.format ~fmt: "%a, %d %b %Y %T GMT" (Netdate.create ~zone: 0 t)

let time_of_string s = Netdate.parse_epoch ?zone: None s

let translate_request url =
  let path = relative_url url in
  match explode_path path with
  | dist :: rest ->
      (try implode_path (Config_file.get dist :: rest), path
       with Not_found ->
	 error_message "No remote repository for %s" dist;
	 raise Not_found)
  | [] ->
      invalid_string_arg "translate_request" url

let reverse_translate url =
  let longest_match k v r =
    if k.[0] <> '$' && is_prefix v url then
      match r with
      | Some (_, repo) as orig ->
          if String.length v > String.length repo then Some (k, v) else orig
      | None -> Some (k, v)
    else
      r
  in
  match Config_file.fold longest_match None with
  | Some (dist, repo) -> dist ^/ substring url ~from: (String.length repo + 1)
  | None -> raise Not_found

type protocol = HTTP | HTTPS | FTP | FILE

let protocol url =
  try
    match String.lowercase_ascii (substring url ~until: (String.index url ':')) with
    | "http" -> HTTP
    | "https" -> HTTPS
    | "ftp" -> FTP
    | "file" -> FILE
    | proto -> invalid_string_arg "unsupported URL protocol" proto
  with Not_found ->
    invalid_string_arg "no protocol in URL" url

let rate_option =
  match String.lowercase_ascii max_rate with
  | "" | "none" | "unlimited" -> ""
  | str -> "--limit-rate " ^ str

let curl_command options url =
  Printf.sprintf
    "%s --fail --silent --header \"Pragma: no-cache\" %s %s %s"
    curl_path rate_option (String.concat " " options) (quoted_string url)

let head_command = curl_command ["--head"]

let iter_headers proc chan =
  let next () =
    try Some (input_line chan)
    with End_of_file -> None
  in
  let rec loop () =
    match next () with
    | Some header ->
        let n = String.length header in
        if n > 0 && header.[n - 1] = '\r' then
          if n > 1 then begin
            proc (String.sub header 0 (n - 1));
            loop ()
          end else () (* CRLF terminates headers *)
        else error_message "Unexpected header: %s" header
    | None -> ()
  in
  loop ()

exception File_not_found
exception Download_error

let process_status = function
  | Unix.WEXITED n -> Printf.sprintf "exited with status %d" n
  | Unix.WSIGNALED _ -> "killed"
  | Unix.WSTOPPED _ -> "stopped"

(* Spawn a curl command and apply a function to its output. *)

let with_curl_process cmd =
  let close chan =
    match Unix.close_process_in chan with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED 22 -> raise File_not_found  (* see curl(1) *)
    | (Unix.WEXITED _ as e) | (Unix.WSIGNALED _ as e) | (Unix.WSTOPPED _ as e) ->
        error_message "Command [%s] %s" cmd (process_status e);
        raise Download_error
  in
  with_resource close Unix.open_process_in cmd

let head url callback =
  let cmd = head_command url in
  debug_message "Command: %s" cmd;
  with_curl_process cmd (iter_headers callback)

let download_command headers header_callback =
  let hdr_opts = List.map (fun h -> "--header " ^ quoted_string h) headers in
  let options =
    match header_callback with
    | Some _ -> "--include" :: hdr_opts
    | None -> hdr_opts
  in
  curl_command options

let iter_body proc chan =
  let len = 4096 in
  let buf = Bytes.create len in
  let rec loop () =
    match input chan buf 0 len with
    | 0 -> ()
    | n -> proc buf 0 n; loop ()
  in
  loop ()

let seq f g x = (f x; g x)

let download url ?(headers=[]) ?header_callback callback =
  let cmd = download_command headers header_callback url in
  debug_message "Command: %s" cmd;
  with_curl_process cmd
    (match header_callback with
     | Some proc -> seq (iter_headers proc) (iter_body callback)
     | None -> iter_body callback)

(* Find the remote URL corresponding to a given relative pathname in the cache,
   or raise Not_found if it does not correspond to a known mapping *)

let translate_file file =
  match explode_path file with
  | dist :: path -> Config_file.get dist ^/ implode_path path
  | _ -> invalid_string_arg "translate_file" file

let download_file file =
  let file' = gensym file in
  let options =
    ["--output"; file'; "--remote-time";
     "--location"; "--max-redirs"; string_of_int max_redirects] @
    (if Sys.file_exists file then
       ["--time-cond"; quoted_string (string_of_time (file_modtime file))]
     else [])
  in
  let cmd = curl_command options (translate_file file) in
  debug_message "Command: %s" cmd;
  if Sys.command cmd = 0 then
    (* file' may not exist if file was not modified *)
    try Sys.rename file' file with _ -> ()
  else begin
    rm file';
    failwith ("cannot download " ^ file)
  end
