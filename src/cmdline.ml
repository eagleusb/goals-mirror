(* Goalfile command line
 * Copyright (C) 2019 Richard W.M. Jones
 * Copyright (C) 2019 Red Hat Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *)

open Printf

open Utils

(* See also "let id" in [lexer.mll]. *)
let var_regexp =
  Str.regexp "\\([a-zA-Z_][-a-zA-Z0-9_]*\\)[ \t]*=[ \t]*\\(.*\\)"

let usage =
  "\
goals: Build software.

 goals [-f Goalfile] ['var=value' ...] ['target' ...]

For detailed help see goals(1).

Options:"

let print_version () =
  printf "%s %s\n" Config.package_name Config.package_version;
  exit 0

(* Get stdlib directory. *)
let datadir =
  try Sys.getenv "GOALS_DATADIR" with Not_found -> Config.datadir
let stdlibdir = datadir // "stdlib"
let prelude_gl_file = stdlibdir // "prelude.gl"
let prelude_sh_file = stdlibdir // "prelude.sh"
let () =
  if not (is_directory stdlibdir) || not (Sys.file_exists prelude_gl_file) then
    failwithf "%s: cannot find the standard library directory, expected %s.  If the standard library directory is in a non-standard location then set GOALS_DATADIR.  If you can trying to run goals from the build directory then use ‘./run goals ...’"
      Sys.executable_name stdlibdir

let input_file,
    debug_flag, directory, includes, nr_jobs, use_prelude, anon_vars, targets =
  let args = ref [] in
  let debug_flag = ref false in
  let directory = ref "." in
  let input_file = ref "Goalfile" in
  let includes = ref [stdlibdir] in
  let add_include dir = includes := dir :: !includes in
  let nr_jobs = ref 4 (* XXX use nproc *) in
  let use_prelude = ref true in

  let argspec = [
    "-C",          Arg.Set_string directory,
                   "directory Change to directory before running";
    "-d",          Arg.Set debug_flag,
                   " Print debug information.";
    "--directory", Arg.Set_string directory,
                   "directory Change to directory before running";
    "-f",          Arg.Set_string input_file,
                   "filename Set name of Goalfile";
    "--file",      Arg.Set_string input_file,
                   "filename Set name of Goalfile";
    "-I",          Arg.String add_include,
                   "dir Add include directory";
    "--include",   Arg.String add_include,
                   "dir Add include directory";
    "-j",          Arg.Set_int nr_jobs,
                   "jobs Set number of parallel jobs";
    "--jobs",      Arg.Set_int nr_jobs,
                   "jobs Set number of parallel jobs";
    "--no-prelude",Arg.Clear use_prelude,
                   " Do not automatically use prelude.gl from stdlib";
    "-v",          Arg.Unit print_version,
                   " Print version and exit";
    "--version",   Arg.Unit print_version,
                   " Print version and exit";
  ] in
  let argspec = Arg.align argspec in
  let anon_fun s = args := s :: !args in
  Arg.parse argspec anon_fun usage;

  let args = List.rev !args in
  let debug_flag = !debug_flag in
  let directory = !directory in
  let input_file = !input_file in
  (* Don't reverse includes - we want newer -I options to take precedence. *)
  let includes = !includes in
  let nr_jobs = !nr_jobs in
  if nr_jobs < 1 then
    failwithf "%s: -j must be >= 1" Sys.executable_name;
  let use_prelude = !use_prelude in

  (* Get the anon var assignments and targets. *)
  let anon_vars, targets =
    List.partition (
      fun arg -> Str.string_match var_regexp arg 0
    ) args in
  let anon_vars =
    List.map (
      fun arg ->
        ignore (Str.string_match var_regexp arg 0);
        let name = Str.matched_group 1 arg in
        let expr = Str.matched_group 2 arg in
        (name, expr)
    ) anon_vars in

  input_file,
  debug_flag, directory, includes, nr_jobs, use_prelude, anon_vars, targets

(* Create the debug function. *)
let debug fs =
  let display str = if debug_flag then prerr_endline str in
  ksprintf display fs
