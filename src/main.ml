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

(* See also "let id" in [lexer.mll]. *)
let var_regexp =
  Str.regexp "\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*=[ \t]*\\(.*\\)"

let usage =
  "\
goals: Build software.

 goals [-f Goalfile] ['var=value' ...] ['target' ...]

For detailed help see goals(1).

Options:"

let main () =
  (* Command line arguments. *)
  let args = ref [] in
  let directory = ref "." in
  let filename = ref "Goalfile" in

  let argspec = [
    "-C",          Arg.Set_string directory,
                   "directory Change to directory before running";
    "--directory", Arg.Set_string directory,
                   "directory Change to directory before running";
    "-f",          Arg.Set_string filename,
                   "filename Set name of Goalfile";
    "--file",      Arg.Set_string filename,
                   "filename Set name of Goalfile";
  ] in
  let argspec = Arg.align argspec in
  let anon_fun s = args := s :: !args in
  Arg.parse argspec anon_fun usage;

  let args = List.rev !args in
  let directory = !directory in
  let filename = !filename in

  (* Parse the input file. *)
  let env = Parse.parse_goalfile filename in

  (* Now we've read the input, change directory. *)
  Sys.chdir directory;

  (* Parse the command line anon args.  Each parameter has the
   * form "name=<expr>" to assign a value to a variable, or
   * "<expr>" to indicate a target to build.
   *)
  let targets = ref [] in
  let env = ref env in
  List.iter (
    fun arg ->
      if Str.string_match var_regexp arg 0 then (
        (* assignment *)
        let name = Str.matched_group 1 arg in
        let expr = Parse.parse_cli_expr (Str.matched_group 2 arg) in
        env := Ast.StringMap.add name expr !env
      )
      else (
        (* target *)
        let expr = Parse.parse_cli_expr arg in
        targets := expr :: !targets
      )
  ) args;
  let targets = List.rev !targets and env = !env in

  (* If no target was set on the command line, use "all ()". *)
  let targets =
    if targets <> [] then targets
    else [Ast.ECall ("all", [])] in

  Ast.print_env stdout env;

  (* Evaluate the target expressions in turn. *)
  Eval.evaluate_targets env targets

let () = main ()
