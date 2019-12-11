(* approx: proxy server for Debian archive files
   Copyright (C) 2017  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Util

let lines_of_channel chan =
  let next () =
    try Some (input_line chan)
    with End_of_file -> None
  in
  let rec loop list =
    match next () with
    | Some line -> loop (line :: list)
    | None -> List.rev list
  in
  loop []

let comment_re = Pcre.regexp "\\s*#.*$"

let remove_comment str = Pcre.qreplace ~rex: comment_re str ~templ: ""

let words_of_line line = Pcre.split (remove_comment line)

let map = ref []

let reset () = map := []

let get_generic convert ?default k =
  try convert (List.assoc k !map)
  with Not_found ->
    (match default with
    | Some v -> v
    | None -> raise Not_found)

let get = get_generic (fun x -> x)

let get_int = get_generic int_of_string

let bool_of_string str =
  match String.lowercase_ascii str with
  | "true"  | "yes" | "on"  | "1" -> true
  | "false" | "no"  | "off" | "0" -> false
  | _ -> failwith ("not a boolean value: " ^ str)

let get_bool = get_generic bool_of_string

let set key value = map := (key, value) :: !map

let fold f init = List.fold_left (fun x (k, v) -> f k v x) init !map

let iter f = fold (fun k v () -> f k v) ()

let read filename =
  let read_file chan =
    let lines = List.map words_of_line (lines_of_channel chan) in
    close_in chan;
    let enter = function
      | [key; value] -> set key value
      | [] -> ()
      | words -> failwith ("malformed line in " ^ filename ^ ": " ^
                           String.concat " " words)
    in
    List.iter enter lines
  in
  with_in_channel open_in filename read_file
