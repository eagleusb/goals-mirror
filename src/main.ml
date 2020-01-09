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

  (* Run the target expressions. *)
  Run.run_targets_to_completion env targets

let () =
  try main ()
  with
  | Failure msg | Sys_error msg ->
     Run.stop_all ();
     prerr_endline ("*** error: " ^ msg);
     exit 1
