(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Bernhard Schommer, AbsInt Angewandte Informatik GmbH       *)
(*                                                                     *)
(*  AbsInt Angewandte Informatik GmbH. All rights reserved. This file  *)
(*  is distributed under the terms of the INRIA Non-Commercial         *)
(*  License Agreement.                                                 *)
(*                                                                     *)
(* *********************************************************************)

open Printf

let search_argv key =
  let len = Array.length Commandline.argv in
  let res: string option ref = ref None in
  for i = 1 to len - 2 do
    if Commandline.argv.(i) = key then
      res := Some Commandline.argv.(i + 1);
  done;
  !res

let absolute_path base file =
  if Filename.is_relative file then
    Filename.concat base file
  else
    file

let (let-) o f = match o with
| Some o -> o
| None -> f ()

(* Locate the .ini file: given by -conf or $COMPCERT_INI, otherwise the
  embedded ini, written once to a content-addressed file in the temp dir *)
let ini_file_name =
  let- () = search_argv "-conf" |> Option.map (absolute_path (Sys.getcwd ())) in
  let- () = Sys.getenv_opt "COMPCERT_INI" in
  let iniContent = IniCrunch.read "compcert.ini" |> Option.get in
  let dir = Filename.get_temp_dir_name () in
  let filename = Filename.concat dir
    ("compcert-" ^ Digest.to_hex (Digest.string iniContent) ^ ".ini") in
  if not (Sys.file_exists filename) then begin
    (* Write to a temp file and rename, so concurrent runs never see a partial file *)
    let tmp, oc = Filename.open_temp_file ~temp_dir:dir "compcert-" ".tmp" in
    output_string oc iniContent;
    close_out oc;
    Sys.rename tmp filename
  end;
  filename

let ini_dir = Filename.dirname ini_file_name

(* Read in the .ini file *)

let _ =
  try
    Readconfig.read_config_file ini_file_name
  with
  | Readconfig.Error(file, lineno, msg) ->
      eprintf "Error reading configuration file %s.\n" ini_file_name;
      eprintf "%s:%d:%s\n" file lineno msg;
      exit 2
  | Sys_error msg ->
      eprintf "Error reading configuration file %s.\n" ini_file_name;
      eprintf "%s\n" msg;
      exit 2

let get_config key =
  match Readconfig.key_val key with
  | Some v -> v
  | None -> eprintf "Configuration option `%s' is not set\n" key; exit 2

let bad_config key vl =
  eprintf "Invalid value `%s' for configuration option `%s'\n"
          (String.concat " " vl) key;
  exit 2

let get_config_string key =
  match get_config key with
  | [v] -> v
  | vl -> bad_config key vl

let get_config_list key =
  match get_config key with
  | [] -> bad_config key []
  | vl -> vl

let tool_absolute_path tools =
  match tools with
  | [] -> []
  | tool::args -> let tool =
      if Filename.is_implicit tool && Filename.dirname tool = Filename.current_dir_name then
        tool
      else
        absolute_path ini_dir tool in
    tool::args

let opt_config_list key =
  match Readconfig.key_val key with
  | Some v -> v
  | None -> []

let prepro =
  tool_absolute_path (get_config_list "prepro")@(opt_config_list "prepro_options")
let asm =
  tool_absolute_path (get_config_list "asm")@(opt_config_list "asm_options")
let linker =
  tool_absolute_path (get_config_list "linker")@(opt_config_list "linker_options")

let get_bool_config key =
  match get_config_string key with
  | "true" -> true
  | "false" -> false
  | v -> bad_config key [v]

let arch =
  match get_config_string "arch" with
  | "powerpc"|"arm"|"x86"|"riscV"|"aarch64" as a -> a
  | v -> bad_config "arch" [v]
let model = get_config_string "model"
let abi = get_config_string "abi"
let is_big_endian =
  match get_config_string "endianness" with
  | "big" -> true
  | "little" -> false
  | v -> bad_config "endianness" [v]
let system = get_config_string "system"
let has_runtime_lib = get_bool_config "has_runtime_lib"
let has_standard_headers = get_bool_config "has_standard_headers"
let stdlib_path =
  if has_runtime_lib then
    let path = get_config_string "stdlib_path" in
    absolute_path ini_dir path
  else
    ""
let asm_supports_cfi = get_bool_config "asm_supports_cfi"

type response_file_style =
  | Gnu         (* responsefiles in gnu compatible syntax *)
  | Diab        (* responsefiles in diab compatible syntax *)
  | Unsupported (* responsefiles are not supported *)

let response_file_style =
  match get_config_string "response_file_style" with
  | "unsupported" -> Unsupported
  | "gnu" -> Gnu
  | "diab" -> Diab
  | v -> bad_config "response_file_style" [v]

let gnu_toolchain = system <> "diab"

let elf_target = system <> "macos" && system <> "cygwin"
