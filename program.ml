(* approx: proxy server for Debian archive files
   Copyright (C) 2011  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf
open Util
open Log

let string_of_uerror = function
  | err, str, "" -> sprintf "%s: %s" str (Unix.error_message err)
  | err, str, arg -> sprintf "%s: %s (%s)" str (Unix.error_message err) arg

let string_of_exception exc =
  match exc with
  | Failure str -> "Failure: " ^ str
  | Invalid_argument str -> "Invalid argument: " ^ str
  | Sys_error str -> str
  | Unix.Unix_error (err, str, arg)-> string_of_uerror (err, str, arg)
  | Control_file.Missing (par, field) ->
      sprintf "File %s, line %d: missing \"%s\" field"
        (Control_file.file_name par)
        (Control_file.line_number par)
        (String.capitalize_ascii field)
  | e -> Printexc.to_string e

let perform f x =
  try f x
  with e -> error_message "%s" (string_of_exception e)

let backtrace () =
  let bt = Printexc.get_backtrace () in
  if bt <> "" then
    let lines = split_lines bt in
    error_message "%s" "Uncaught exception";
    List.iter (fun s ->  if s <> "" then error_message "  %s" s) lines

let main_program f x =
  try f x
  with e ->
    backtrace ();
    error_message "%s" (string_of_exception e);
    exit 1

let print fmt = error_message fmt

let file_message file msg = print "%s: %s" (Config.shorten file) msg
