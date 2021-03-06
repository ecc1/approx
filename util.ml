(* approx: proxy server for Debian archive files
   Copyright (C) 2017  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf

module U = Unix
module ULF = U.LargeFile

let invalid_string_arg msg arg = invalid_arg (msg ^ ": " ^ arg)

let is_prefix pre str =
  let prefix_len = String.length pre in
  let string_len = String.length str in
  let rec loop i =
    if i = prefix_len then true
    else if i = string_len || pre.[i] <> str.[i] then false
    else loop (i + 1)
  in
  loop 0

let substring ?(from=0) ?until str =
  let n = String.length str in
  let until = match until with Some i -> i | None -> n in
  if from = 0 && until = n then str
  else String.sub str from (until - from)

let split sep str =
  let next i =
    try Some (String.index_from str i sep)
    with Not_found -> None
  in
  let rec loop acc i =
    match next i with
    | Some j -> loop (substring str ~from: i ~until: j :: acc) (j + 1)
    | None -> substring str ~from: i :: acc
  in
  List.rev (loop [] 0)

let join sep list =
  String.concat (String.make 1 sep) list

let split_lines = split '\n'

let explode_path = split '/'

let implode_path = join '/'

let (^/) = Filename.concat

let remove_leading c str =
  let n = String.length str in
  let rec loop i =
    if i = n then ""
    else if str.[i] <> c then substring str ~from: i
    else loop (i + 1)
  in
  loop 0

let remove_trailing c str =
  let rec loop i =
    if i < 0 then ""
    else if str.[i] <> c then substring str ~until: (i + 1)
    else loop (i - 1)
  in
  loop (String.length str - 1)

let make_directory path =
  (* Create a directory component in the path. Since it might be
     created concurrently, we have to ignore the Unix EEXIST error:
     simply testing for existence first introduces a race condition. *)
  let make_dir name =
    try U.mkdir name 0o755
    with U.Unix_error (U.EEXIST, _, _) ->
      if not (Sys.is_directory name) then
        failwith ("file " ^ name ^ " is not a directory")
  in
  let rec loop cwd = function
    | dir :: rest ->
        let name = cwd ^/ dir in
        make_dir name;
        loop name rest
    | [] -> ()
  in
  match explode_path path with
  | "" :: dirs -> loop "/" dirs
  | dirs -> loop "." dirs

let quoted_string = sprintf "%S"

let relative_path path =
  let n = String.length path in
  let rec loop i =
    if i = n then "."
    else if path.[i] <> '/' then String.sub path i (n - i)
    else loop (i + 1)
  in
  loop 0

let relative_url path =
  try
    if path.[0] = '/' then relative_path path
    else
      let i = String.index path ':' in
      if path.[i + 1] = '/' && path.[i + 2] = '/' then
        let j = String.index_from path (i + 3) '/' in
        relative_path (substring path ~from: j)
      else
        raise Exit
  with _ ->
    failwith ("malformed URL: " ^ path)

let split_extension file =
  (* look for '.' in basename only, not parent directories *)
  let left = try String.rindex file '/' with Not_found -> -1 in
  try
    let i = String.rindex file '.' in
    if i > left then (substring file ~until: i, substring file ~from: i)
    else (file, "")
  with Not_found -> (file, "")

(* Return a filename with its extension, if any, removed *)

let without_extension file = fst (split_extension file)

let extension file = snd (split_extension file)

(* private exception to wrap any exception raised during cleanup action *)

exception Unwind of exn

let unwind_protect body post =
  try
    let result = body () in
    try post (); result with e -> raise (Unwind e)
  with
  | Unwind e -> raise e    (* assume cleanup has been done *)
  | e -> post (); raise e

let with_resource release acquire x f =
  let res = acquire x in
  unwind_protect
    (fun () -> f res)
    (fun () -> release res)

let with_in_channel openf = with_resource close_in openf

let with_out_channel openf = with_resource close_out openf

let gensym str =
  sprintf "%s.%d.%09.0f"
    (without_extension str)
    (U.getpid ())
    (fst (modf (U.gettimeofday ())) *. 1e9)

(* Use the default temporary directory unless it has been set
   to something inaccessible, in which case use "/tmp" *)

let tmp_dir_name = ref None

(* Return the name of the temporary file directory *)

let tmp_dir () =
  match !tmp_dir_name with
  | Some dir -> dir
  | None ->
      let dir =
        try
          let dir = Filename.get_temp_dir_name () in
          U.access dir [U.R_OK; U.W_OK; U.X_OK];
          dir
        with U.Unix_error _ -> "/tmp"
      in
      tmp_dir_name := Some dir;
      dir

let rm file = try Sys.remove file with _ -> ()

(* Decompression programs for supported compression formats *)

let decompressors =
  [".bz2", "/bin/bzcat";
   ".gz", "/bin/zcat";
   ".xz", "/usr/bin/xzcat"]

let compressed_extensions = List.map fst decompressors

(* Check if a file is compressed by examining its extension *)

let is_compressed file = List.mem (extension file) compressed_extensions

(* Decompress a file to a temporary file,
   rather than reading from a pipe or using the CamlZip library,
   so that we detect corrupted files before partially processing them.
   This is also significantly faster than using CamlZip.
   Return the temporary file name, which must be removed by the caller *)

let decompress file =
  let prog =
    try List.assoc (extension file) decompressors
    with Not_found -> invalid_string_arg "decompress" file
  in
  let tmp = (tmp_dir ()) ^/ gensym (Filename.basename file) in
  let cmd = sprintf "%s %s > %s" prog file tmp in
  if Sys.command cmd = 0 then tmp
  else (rm tmp; failwith ("decompress " ^ file))

let with_decompressed file = with_resource rm decompress file

let decompress_and_apply f file =
  if is_compressed file then with_decompressed file f
  else f file

(* Return a channel for reading a possibly compressed file. *)

let open_file = decompress_and_apply open_in

let compressed_versions name =
  if is_compressed name then invalid_string_arg "compressed_versions" name;
  name :: List.map (fun ext -> name ^ ext) compressed_extensions

let stat_file file = try Some (ULF.stat file) with U.Unix_error _ -> None

let is_cached_nak name =
  match stat_file name with
  | Some { ULF.st_size = 0L; st_perm = 0; _ } -> true
  | _ -> false

let file_size file = (ULF.stat file).ULF.st_size

let file_modtime file = (ULF.stat file).ULF.st_mtime

let file_ctime file = (ULF.stat file).ULF.st_ctime

let minutes_old t = int_of_float ((U.time () -. t) /. 60. +. 0.5)

let newest_file list =
  let newest cur name =
    match stat_file name with
    | None | Some { ULF.st_size = 0L; st_perm = 0; _ } (* cached NAK *) -> cur
    | Some { ULF.st_mtime = modtime; _ } ->
        begin match cur with
        | Some (_, t) -> if modtime > t then Some (name, modtime) else cur
        | None -> Some (name, modtime)
        end
  in
  match List.fold_left newest None list with
  | Some (f, _) -> f
  | None -> raise Not_found

let open_out_excl file =
  U.out_channel_of_descr (U.openfile file [U.O_CREAT; U.O_WRONLY; U.O_EXCL] 0o644)

let with_temp_file name proc =
  let file = gensym name in
  with_out_channel open_out_excl file proc;
  file

let update_ctime name =
  match stat_file name with
  | Some { ULF.st_atime = atime; st_mtime = mtime; _ } -> U.utimes name atime mtime
  | None -> ()

let directory_id name =
  match stat_file name with
  | Some s ->
      if s.ULF.st_kind = U.S_DIR then
	Some (s.ULF.st_dev, s.ULF.st_ino)
      else
	None
  | None -> None

let fold_fs_tree non_dirs f init path =
  let rec walk uids_seen init path =
    let visit uids acc name =
      walk uids acc (path ^/ name)
    in
    let uid = directory_id path in
    if uid <> None then
      if List.mem uid uids_seen then (* cycle detected *)
        init
      else
        let uids_seen = uid :: uids_seen in
        let children = try Sys.readdir path with _ -> [||] in
        let init = if non_dirs then init else f init path in
        Array.fold_left (visit uids_seen) init children
    else if non_dirs && Sys.file_exists path then
      f init path
    else
      init
  in
  walk [] init path

let fold_dirs f = fold_fs_tree false f

let fold_non_dirs f = fold_fs_tree true f

let iter_of_fold fold proc = fold (fun () -> proc) ()

let iter_dirs = iter_of_fold fold_dirs

let iter_non_dirs = iter_of_fold fold_non_dirs

module type MD =
  sig
    type t
    val file : string -> t
    val to_hex : t -> string
  end

module FileDigest (MsgDigest : MD) =
  struct
    let sum file = MsgDigest.to_hex (MsgDigest.file file)
  end

let file_md5sum = let module F = FileDigest(Digest) in F.sum
let file_sha1sum = let module F = FileDigest(Sha1) in F.sum
let file_sha256sum = let module F = FileDigest(Sha256) in F.sum

let user_id =
  object
    method kind = "user"
    method get = U.getuid
    method set = U.setuid
    method lookup x = (U.getpwnam x).U.pw_uid
  end

let group_id =
  object
    method kind = "group"
    method get = U.getgid
    method set = U.setgid
    method lookup x = (U.getgrnam x).U.gr_gid
  end

let drop_privileges ~user ~group =
  let drop id name =
    try id#set (id#lookup name)
    with
    | Not_found -> failwith ("unknown " ^ id#kind ^ " " ^ name)
    | U.Unix_error (U.EPERM, _, _) ->
        failwith (Sys.argv.(0) ^ " must be run by root"
                  ^ (if user <> "root" then " or by " ^ user else ""))
  in
  (* change group first, since we must still be privileged to change user *)
  drop group_id group;
  drop user_id user

let check_id ~user ~group =
  let check id name =
    try
      if id#get () <> id#lookup name then
        failwith ("not running as " ^ id#kind ^ " " ^ name)
    with Not_found -> failwith ("unknown " ^ id#kind ^ " " ^ name)
  in
  check user_id user;
  check group_id group

let string_of_sockaddr sockaddr ~with_port =
  match sockaddr with
  | U.ADDR_INET (host, port) ->
      let addr = U.string_of_inet_addr host in
      if with_port then sprintf "%s port %d" addr port else addr
  | U.ADDR_UNIX path ->
      failwith ("Unix domain socket " ^ path)
