(* approx: proxy server for Debian archive files
   Copyright (C) 2014  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* Translate a request URL to the remote repository URL and
   return it together with the relative path for the cache *)

val translate_request : string -> string * string

(* Translate a remote URL back to a relative path for the cache *)

val reverse_translate : string -> string

type protocol = HTTP | HTTPS | FTP | FILE

val protocol : string -> protocol

exception File_not_found  (* raised when remote server returns 404 *)
exception Download_error  (* raised when any other failure occurs *)

(* Perform HTTP HEAD (or equivalent for FTP and FILE) on the given URL
   and apply a callback to each header that is returned *)

val head : string -> (string -> unit) -> unit

(* Download the specified URL with optional request headers,
   then apply callbacks to the headers and body chunks *)

val download :
  string ->
  ?headers:string list ->
  ?header_callback:(string -> unit) ->
  (bytes -> int -> int -> unit) -> unit

(* Download a file from a remote repository *)

val download_file : string -> unit

(* Format and parse HTTP-compliant times *)

val time_of_string : string -> float

val string_of_time : float -> string
