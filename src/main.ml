(* Goalfile parser
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

(* See comment in parser.mly. *)
let () =
  Parser.lexer_read := Some Lexer.read;
  Parser.eval_substitute := Some Eval.substitute

let main () =
  (* Handle the command line. *)
  let anon_vars, targets = Cmdline.parse () in

  (* Change directory (-C option). *)
  Sys.chdir (Cmdline.directory ());

  (* Create a temporary directory which is always cleaned up at exit. *)
  let tmpdir =
    let temp_dir = try Unix.getenv "TMPDIR" with Not_found -> "/var/tmp" in
    let t = Filename.temp_file ~temp_dir "goals" ".d" in
    Unix.unlink t;
    Unix.mkdir t 0o700;
    at_exit (
      fun () ->
        let cmd = sprintf "rm -rf %s" (Filename.quote t) in
        ignore (Sys.command cmd)
    );
    t in

  (* Create the initial environment, containing the system environment
   * and a few other standard strings.
   *)
  let env =
    Array.fold_left (
      fun env environ ->
        let k, v = split "=" environ in
        Ast.Env.add k (Ast.EConstant (Ast.noloc, Ast.CString v)) env
    ) Ast.Env.empty (Unix.environment ()) in
  let env =
    Ast.Env.add "tmpdir" (Ast.EConstant (Ast.noloc, Ast.CString tmpdir)) env in
  let env =
    Ast.Env.add "stdlib"
      (Ast.EConstant (Ast.noloc, Ast.CString Cmdline.stdlibdir))
      env in
  (*let env =
    if Cmdline.debug_flag then Ast.Env.add "debug" (Ast.EConstant (noloc, Ast.CBool true)) env else env in *)

  (* Parse the prelude. *)
  let env =
    if Cmdline.use_prelude () then
      Parse.parse_goalfile env Cmdline.prelude_gl_file
    else env in

  (* Parse the input file. *)
  let env = Parse.parse_goalfile env (Cmdline.input_file ()) in

  (* Parse the command line assignments. *)
  let env =
    List.fold_left (
      fun env (name, expr) ->
        let expr = Parse.parse_expr "commandline" expr in
        Ast.Env.add name expr env
    ) env anon_vars in

  (* Parse the target expressions. *)
  let targets = List.map (Parse.parse_expr "commandline") targets in

  (* If no target was set on the command line, use "all ()". *)
  let targets =
    if targets <> [] then targets
    else [Ast.ECall (Ast.noloc, "all", [])] in

  if Cmdline.debug_flag () then
    Ast.print_env stderr env;

  (* Construct the dependency DAG with the root(s) being the targets. *)
  let dag = Deps.create env targets in

  (* Run the jobs. *)
  let state = Deps.new_state dag Run.goal_runner Run.exists_runner in
  let next_job () = Deps.next_job state in
  let retire_job job = Deps.retire_job state job in
  let fail_job job = Deps.fail_job state job in
  let string_of_job job = Deps.string_of_job job in
  Jobs.run next_job retire_job fail_job string_of_job

let () =
  try main ()
  with
  | Failure msg | Sys_error msg ->
     prerr_endline ("*** error: " ^ msg);
     exit 1
