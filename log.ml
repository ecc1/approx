(* approx: proxy server for Debian archive files
   Copyright (C) 2013  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf

let stderr_log _ msg =
  prerr_string msg;
  flush stderr

let writer = ref stderr_log

let log_to_stderr () =
  writer := stderr_log

let facility_of_string s =
  match String.lowercase_ascii s with
  | "authpriv" -> `Authpriv
  | "cron" -> `Cron
  | "daemon" -> `Daemon
  | "ftp" -> `Ftp
  | "kern" -> `Kern
  | "local0" -> `Local0
  | "local1" -> `Local1
  | "local2" -> `Local2
  | "local3" -> `Local3
  | "local4" -> `Local4
  | "local5" -> `Local5
  | "local6" -> `Local6
  | "local7" -> `Local7
  | "lpr" -> `Lpr
  | "mail" -> `Mail
  | "news" -> `News
  | "syslog" -> `Syslog
  | "user" -> `User
  | "uucp" -> `Uucp
  | "default" -> `Default
  | _ -> Util.invalid_string_arg "syslog facility" s

let log_to_syslog () =
  let facility = facility_of_string Config.syslog in
  let ident =
    sprintf "%s[%d]" (Filename.basename Sys.argv.(0)) (Unix.getpid ())
  in
  Netsys_posix.openlog (Some ident) [] facility;
  writer := Netsys_posix.syslog facility

let message enabled level =
  (* ensure message is newline-terminated,
     otherwise syslog-ng behaves differently than syslog *)
  let terminate str =
    let n = String.length str in
    if n = 0 || str.[n - 1] <> '\n' then str ^ "\n"
    else str
  in
  ksprintf (fun str -> if enabled then !writer level (terminate str))

let error_message fmt = message true `Err fmt
let info_message fmt = message Config.verbose `Info fmt
let debug_message fmt = message Config.debug `Debug fmt
