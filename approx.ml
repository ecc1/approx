(* approx: proxy server for Debian archive files
   Copyright (C) 2017  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf

module U = Unix
module ULF = U.LargeFile

open Config
open Log
open Program
open Util

(* Hint that a download is in progress *)

let in_progress name = name ^ ".hint"

let wait_for_download_in_progress name =
  let hint = in_progress name in
  let timeout = float_of_int max_wait in
  let rec wait n =
    match stat_file hint with
    | Some { ULF.st_mtime = mtime; _ } ->
        if U.time () -. mtime > timeout then begin
          error_message "Concurrent download of %s is taking too long" name;
          (* remove the other process's hint file if it still exists,
             so we can create our own *)
          rm hint
        end else begin
          if n = 0 then
            debug_message "Waiting for concurrent download of %s" name;
          U.sleep 1;
          wait (n + 1)
        end
    | None -> ()
  in
  wait 0

let debug_headers msg headers =
  debug_message "%s" msg;
  List.iter (fun (x, y) -> debug_message "  %s: %s" x y) headers

let proxy_headers size modtime =
  let headers =
    ["Content-Type", "text/plain";
     "Content-Length", Int64.to_string size]
  in
  if modtime = 0. then headers
  else ("Last-Modified", Url.string_of_time modtime) :: headers

type local_status =
  | Done of Nethttpd_types.http_service_reaction
  | Cache_miss of float

let head_request env = env#cgi_request_method = "HEAD"

(* Deliver a file from the local cache *)

let deliver_local name env =
  debug_message "  => delivering from cache";
  let size = file_size name in
  env#set_output_header_fields (proxy_headers size (file_modtime name));
  debug_headers "Local response" env#output_header_fields;
  let file = if head_request env then "/dev/null" else cache_dir ^/ name in
  Done (`File (`Ok, None, file, 0L, size))

let not_modified () =
  debug_message "  => not modified";
  Done (`Std_response (`Not_modified, None, None))

let nak () =
  debug_message "  => not found (cached)";
  Done (`Std_response (`Not_found, None, None))

(* The modification time (mtime) tells when the contents of the file
   last changed, and is used by the "If-Modified-Since" logic.

   The last status change time (ctime) is used to indicate when a file
   was last "verified" by contacting the remote repository.

   Whenever we learn that the file is still valid via a "Not Modified"
   response, we update the ctime so that the file will continue to be
   considered current. *)

let print_age mod_time ctime =
  if debug then begin
    debug_message "  last modified: %s" (Url.string_of_time mod_time);
    debug_message "  last verified: %s" (Url.string_of_time ctime)
  end

(* "File not found" or NAK responses are cached as empty files with
   permissions = 0. Create a cached NAK as an empty temp file, set
   its permissions, then atomically rename it. *)

let cache_nak file =
  debug_message "  caching \"file not found\"";
  make_directory (Filename.dirname file);
  let tmp_file = gensym file in
  let chan = open_out_excl tmp_file in
  close_out chan;
  U.chmod tmp_file 0;
  Sys.rename tmp_file file

(* Attempt to serve the requested file from the local cache.
   Deliver immutable files and valid index files from the cache.
   Deliver Release files if they are not too old.
   Otherwise contact the remote repository. *)

let serve_local name ims env =
  wait_for_download_in_progress name;
  match stat_file name with
  | Some { ULF.st_mtime = mod_time; st_ctime = ctime;
           st_size = size; st_perm = perm; _ } ->
      let deliver_if_newer () =
        if mod_time > ims then deliver_local name env
        else not_modified ()
      in
      if size = 0L && perm = 0 then begin (* cached NAK *)
        debug_message "  cached \"file not found\"";
        print_age mod_time ctime;
        if minutes_old ctime <= interval then nak ()
        else Cache_miss mod_time
      end else if Release.is_release name then begin
        print_age mod_time ctime;
        if minutes_old ctime <= interval then deliver_if_newer ()
        else Cache_miss mod_time
      end else if Release.immutable name || Release.valid name then
        deliver_if_newer ()
      else begin
        print_age mod_time ctime;
        if minutes_old ctime <= interval then deliver_if_newer ()
        else Cache_miss mod_time
      end
  | None ->
      Cache_miss 0.

let create_hint name =
  make_directory (Filename.dirname name);
  U.close (U.openfile (in_progress name) [U.O_CREAT; U.O_WRONLY] 0o644)

let remove_hint name = rm (in_progress name)

type cache_info = { file : string; tmp_file : string; chan : out_channel }

type cache_state =
  | Cache of cache_info
  | Pass_through
  | Undefined

(* Don't cache the result of a request for a directory *)

let should_pass_through name =
  if Sys.file_exists name then Sys.is_directory name
  else
    let n = String.length name in
    n = 0 || name.[n - 1] = '/' || not (String.contains name '/')

let open_cache file =
  if should_pass_through file then begin
    debug_message "  pass-through %s" file;
    Pass_through
  end else
    try
      debug_message "  open cache %s" file;
      make_directory (Filename.dirname file);
      let tmp_file = gensym file in
      let chan = open_out_excl tmp_file in
      Cache { file = file; tmp_file = tmp_file; chan = chan }
    with e ->
      error_message "Cannot cache %s" file;
      raise e

let write_cache cache str pos len =
  match cache with
  | Cache { chan = chan; _ } -> output chan str pos len
  | Pass_through -> ()
  | Undefined -> assert false

exception Wrong_size

let close_cache cache size mod_time =
  match cache with
  | Cache { file = file; tmp_file = tmp_file; chan = chan } ->
      debug_message "  close cache %s" file;
      close_out chan;
      if size = -1L || size = file_size tmp_file then begin
        if mod_time <> 0. then begin
          debug_message "  setting mtime to %s" (Url.string_of_time mod_time);
          U.utimes tmp_file mod_time mod_time
        end;
        Sys.rename tmp_file file
      end else begin
        error_message "Size of %s should be %Ld, not %Ld"
          file size (file_size tmp_file);
        rm tmp_file;
        raise Wrong_size
      end
  | Pass_through -> ()
  | Undefined -> assert false

let remove_cache cache =
  match cache with
  | Cache { tmp_file = tmp_file; chan = chan; _ } ->
      close_out chan;
      error_message "Removing %s (size: %Ld)" tmp_file (file_size tmp_file);
      rm tmp_file
  | Pass_through | Undefined -> ()

type download_status =
  | Delivered
  | Cached
  | Not_modified
  | Redirect of string
  | File_not_found
  | Download_error

let string_of_download_status = function
  | Delivered -> "delivered"
  | Cached -> "cached"
  | Not_modified -> "not modified"
  | Redirect url -> "redirected to " ^ url
  | File_not_found -> "not found"
  | Download_error -> "download error"

type response_state =
  { name : string;
    mutable status : int;
    mutable length : int64;
    mutable last_modified : float;
    mutable location : string;
    mutable content_type : string;
    mutable body_seen : bool;
    mutable cache : cache_state }

let new_response url name =
  { name = name;
    status = 0;
    length = -1L;
    last_modified = 0.;
    location = url;
    content_type = "text/plain";
    body_seen = false;
    cache = Undefined }

type cgi = Netcgi.cgi_activation

let send_header size modtime (cgi : cgi) =
  let headers = proxy_headers size modtime in
  let fields = List.map (fun (name, value) -> (name, [value])) headers in
  cgi#set_header ~status: `Ok ~fields ();
  debug_headers "Proxy response" cgi#environment#output_header_fields

let pass_through_header resp (cgi : cgi) =
  let fields = ["Content-Type", [resp.content_type]] in
  let fields =
    if resp.length < 0L then fields
    else ("Content-Length", [Int64.to_string resp.length]) :: fields
  in
  cgi#set_header ~status: `Ok ~fields ();
  debug_headers "Pass-through response" cgi#environment#output_header_fields

let finish_delivery resp =
  close_cache resp.cache resp.length resp.last_modified;
  if resp.length >= 0L || resp.cache = Pass_through then Delivered else Cached

let finish_head resp cgi =
  send_header resp.length resp.last_modified cgi;
  Delivered

let with_pair rex str proc =
  match Pcre.extract ~rex ~full_match: false str with
  | [| a; b |] -> proc (a, b)
  | _ -> assert false

let status_re = Pcre.regexp "^HTTP/\\d+(?:\\.\\d+)?\\s+(\\d{3})(?:\\s+(.*?)\\s*)?$"
let header_re = Pcre.regexp "^(.*?):\\s*(.*?)\\s*$"

let process_header resp str =
  let do_status (code, _) =
    resp.status <- int_of_string code
  in
  let do_header (header, value) =
    match String.lowercase_ascii header with
    | "content-length" ->
        (try resp.length <- Int64.of_string value
         with Failure _ ->
           error_message "Cannot parse Content-Length %s" value)
    | "last-modified" ->
        (try resp.last_modified <- Url.time_of_string value
         with Invalid_argument _ ->
           error_message "Cannot parse Last-Modified date %s" value)
    | "location" ->
        (try resp.location <- Neturl.string_of_url (Neturl.parse_url value)
         with Neturl.Malformed_URL ->
           error_message "Cannot parse Location %s" value)
    | "content-type" -> (* only used for pass-through content *)
        resp.content_type <- value
    | _ -> ()
  in
  debug_message "  %s" str;
  try with_pair header_re str do_header
  with Not_found -> (* e.g., status line or CRLF *)
    try with_pair status_re str do_status
    with Not_found -> error_message "Unrecognized response: %s" str

(* Process a chunk of the response body.
   If no Content-Length was present in the header, we cache the whole
   file before delivering it to the client. The alternative -- using
   chunked transfer encoding -- triggers a bug in APT. *)

let process_body resp cgi str pos len =
  if resp.status = 200 then begin
    if not resp.body_seen then begin
      resp.body_seen <- true;
      assert (resp.cache = Undefined);
      resp.cache <- open_cache resp.name;
      if resp.cache = Pass_through then
        pass_through_header resp cgi
      else if resp.length >= 0L then
        send_header resp.length resp.last_modified cgi
    end;
    write_cache resp.cache str pos len;
    if resp.length >= 0L || resp.cache = Pass_through then
      (* stream the data back to the client as we receive it *)
      cgi#output#really_output str pos len
  end

(* Download a file from an HTTP or HTTPS repository *)

let download_http resp url ims cgi =
  let headers =
    if ims > 0. then ["If-Modified-Since: " ^ Url.string_of_time ims] else []
  in
  let header_callback = process_header resp in
  let body_callback = process_body resp cgi in
  let is_head = head_request cgi#environment in
  let rec loop redirects =
    resp.status <- 0;
    if is_head then
      Url.head resp.location header_callback
    else
      Url.download resp.location ~headers ~header_callback body_callback;
    match resp.status with
    | 200 -> if is_head then finish_head resp cgi else finish_delivery resp
    | 304 -> Not_modified
    | 301 | 302 | 303 | 307 ->
        if should_pass_through (relative_url resp.location) then begin
          (* the request was redirected to content that should not be cached,
             like a directory listing *)
          remove_cache resp.cache;
          Redirect resp.location
        end else if redirects >= max_redirects then begin
          error_message "Too many redirections for %s" url;
          File_not_found
        end else
          loop (redirects + 1)
    | 404 -> File_not_found
    | n -> error_message "Unexpected status code: %d" n; Download_error
  in
  loop 0

(* Download a file from an FTP repository *)

let download_ftp resp url ims cgi =
  Url.head url (process_header resp);
  let mod_time = resp.last_modified in
  debug_message "  ims %s  mtime %s"
    (Url.string_of_time ims) (Url.string_of_time mod_time);
  if 0. < mod_time && mod_time <= ims then Not_modified
  else if head_request cgi#environment then finish_head resp cgi
  else begin
    resp.status <- 200; (* for process_body *)
    Url.download url (process_body resp cgi);
    finish_delivery resp
  end

let download_url url name ims cgi =
  let dl =
    match Url.protocol url with
    | Url.HTTP | Url.HTTPS -> download_http
    | Url.FTP | Url.FILE -> download_ftp
  in
  let resp = new_response url name in
  try
    create_hint name;
    unwind_protect
      (fun () -> dl resp url ims cgi)
      (fun () -> remove_hint name)
  with e ->
    remove_cache resp.cache;
    match e with
    | Url.File_not_found -> File_not_found
    | Url.Download_error -> Download_error
    | e -> info_message "%s" (string_of_exception e); Download_error

(* Handle any processing triggered by downloading a given file *)

let updates_needed = ref []

let cleanup_after file =
  if pdiffs && Release.is_pdiff file then
    (* record the affected index for later update *)
    let index = Pdiff.index_file file in
    if not (List.mem index !updates_needed) then begin
      debug_message "Deferring pdiffs for %s" index;
      updates_needed := index :: !updates_needed
    end

let copy_to dst src =
  let len = 4096 in
  let buf = Bytes.create len in
  let rec loop () =
    match input src buf 0 len with
    | 0 -> ()
    | n -> dst#really_output buf 0 n; loop ()
  in
  loop ()

(* Similar to deliver_local, but we have to copy it ourselves *)

let copy_from_cache name cgi =
  wait_for_download_in_progress name;
  send_header (file_size name) (file_modtime name) cgi;
  let output = cgi#output in
  if not (head_request cgi#environment) then
    with_in_channel open_in name (copy_to output);
  output#commit_work ()

(* Update the ctime but not the mtime of the file *)

let update_ctime name =
  match stat_file name with
  | Some { ULF.st_atime = atime; st_mtime = mtime; st_ctime = ctime; _ } ->
      U.utimes name atime mtime;
      if debug then
        debug_message "  updated ctime to %s" (Url.string_of_time ctime)
  | None -> ()

let redirect url (cgi : cgi) =
  let url' =
    try
      let path = Url.reverse_translate url in
      cgi#url ~with_script_name: `None ~with_path_info: (`This path) ()
    with Not_found -> url
  in
  new Netmime.basic_mime_header ["Location", url']

let serve_remote url name ims mod_time cgi =
  let respond ?header code =
    raise (Nethttpd_types.Standard_response (code, header, None))
  in
  let copy_if_newer () =
    (* deliver the cached copy if it is newer than the client's *)
    if mod_time > ims then copy_from_cache name cgi
    else respond `Not_modified
  in
  let status = download_url url name (max ims mod_time) cgi in
  info_message "%s: %s" url (string_of_download_status status);
  match status with
  | Delivered ->
      cgi#output#commit_work ();
      if not (head_request cgi#environment) then cleanup_after name
  | Cached ->
      copy_from_cache name cgi;
      cleanup_after name
  | Not_modified ->
      update_ctime name;
      copy_if_newer ()
  | Redirect url' ->
      respond `Found ~header: (redirect url' cgi)
  | File_not_found ->
      if is_cached_nak name then begin
        update_ctime name;
        respond `Not_found
      end else if offline && Sys.file_exists name then copy_if_newer ()
      else begin
        cache_nak name;
        respond `Not_found
      end
  | Download_error ->
      if not (is_cached_nak name) && offline && Sys.file_exists name then
        copy_if_newer ()
      else
        respond `Not_found

let remote_service url name ims mod_time =
  object
    method process_body _ =
      object
        method generate_response env =
          let cgi =
            (* buffered activation runs out of memory on large downloads *)
            Nethttpd_services.std_activation `Std_activation_unbuffered env
          in
          serve_remote url name ims mod_time cgi
      end
  end

(* Handle a cache miss, either because the file is not present (mod_time = 0)
   or it hasn't been verified recently enough *)

let cache_miss url name ims mod_time =
  debug_message "  => cache miss";
  `Accept_body (remote_service url name ims mod_time)

let ims_time env =
  try Netdate.parse_epoch (env#input_header#field "If-Modified-Since")
  with Not_found | Invalid_argument _ -> 0.

let server_error e =
  backtrace ();
  `Std_response (`Internal_server_error, None, Some (string_of_exception e))

let static env str =
  `Static (`Ok, None, if head_request env then "" else str)

let serve_file env =
  (* handle URL-encoded '+', '~', etc. *)
  match Netencoding.Url.decode ~plus: false env#cgi_request_uri with
  | "/" -> static env Config.index
  | "/robots.txt" -> static env "User-agent: *\nDisallow: /\n"
  | path ->
      begin
	try
	  let url, name = Url.translate_request path in
	  if should_pass_through name then cache_miss url name 0. 0.
	  else
            let ims = ims_time env in
            match serve_local name ims env with
            | Done reaction -> reaction
            | Cache_miss mod_time -> cache_miss url name ims mod_time
	with
	| Not_found -> `Std_response (`Not_found, None, None)
	| e -> server_error e
      end

let process_request env =
  debug_message "Connection from %s"
    (string_of_sockaddr env#remote_socket_addr ~with_port: true);
  let meth = env#cgi_request_method in
  debug_headers (sprintf "Request: %s %s" meth env#cgi_request_uri)
    env#input_header_fields;
  if (meth = "GET" || meth = "HEAD") && env#cgi_query_string = "" then
    serve_file env
  else
    `Std_response (`Forbidden, None, Some "invalid HTTP request")

let error_response info =
  let code = info#response_status_code in
  let msg =
    string_of_int code ^ ": " ^
      try Nethttp.string_of_http_status (Nethttp.http_status_of_int code)
      with Not_found -> "???"
  in
  let detail =
    match info#error_message with
    | "" -> ""
    | s -> "<p>" ^ s ^ "</p>"
  in
  sprintf "<html><body><h1>%s</h1>%s</body></html>" msg detail

open Nethttpd_reactor

let config =
  object
    inherit modify_http_reactor_config default_http_reactor_config
    (* changes from default_http_protocol_config *)
    method! config_announce_server = `Ocamlnet_and ("approx/" ^ version)
    (* changes from default_http_processor_config *)
    method! config_error_response = error_response
    method! config_log_error _ msg = error_message "%s" msg
  end

let proxy_service =
  object
    method name = "proxy_service"
    method def_term = `Proxy_service
    method print fmt = Format.fprintf fmt "%s" "proxy_service"
    method process_header = process_request
  end

let approx () =
  log_to_syslog ();
  check_id ~user ~group;
  Sys.chdir cache_dir;
  U.set_nonblock U.stdin;
  Nethttpd_reactor.process_connection config U.stdin proxy_service;
  List.iter Pdiff.update !updates_needed

let () = main_program approx ()
