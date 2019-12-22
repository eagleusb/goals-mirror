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
let var_regexp = Str.regexp "\\([a-zA-Z_][a-zA-Z0-9_]*\\)=\\(.*\\)"

let usage =
  "\
goals: Build software.

 goals [-f Goalfile] ['var=value' ...] ['target' ...]

For detailed help see goals(1).

Options:"

let main () =
  (* Command line arguments. *)
  let filename = ref "Goalfile" in

  let argspec = [
    "-f",        Arg.Set_string filename,
                 "filename Set name of Goalfile";
    "--file",    Arg.Set_string filename,
                 "filename Set name of Goalfile";
  ] in
  let argspec = Arg.align argspec in
  let args = ref [] in
  let anon_fun s = args := s :: !args in
  Arg.parse argspec anon_fun usage;

  (*let args = List.rev !args in*)
  let filename = !filename in

  (* Parse the input file. *)
  let file = Parse.parse_goalfile filename in

  (* Parse the command line anon args.  Each parameter has the
   * form "name=<expr>" to assign a value to a variable, or
   * "<expr>" to indicate a target to build.
   *)
  let assignments = ref [] in
  let targets = ref [] in
  List.iter (
    fun arg ->
      if Str.string_match var_regexp arg 0 then ( (* assignment *)
        let name = Str.matched_group 1 arg in
        let expr = Parse.parse_cli_expr (Str.matched_group 2 arg) in
        assignments := Ast.Let (name, expr) :: !assignments
      )
      else ( (* target *)
        let expr = Parse.parse_cli_expr arg in
        targets := expr :: !targets
      )
  ) !args;

  (* If no target was set on the command line, find
   * the first goal in the file.
   *)
  if !targets = [] then (
    try
      let first_goal =
        List.find (function Ast.Goal _ -> true | _ -> false) file in
      match first_goal with
      | Ast.Goal (name, [], _, _, _) ->
         targets := [Ast.ECall (name, [])]
      | Ast.Goal (name, _, _, _, _) ->
         failwithf "%s: first target ‘%s’ has parameters and so cannot be used as the default target"
           filename name
      | _ -> assert false
    with
      (* Actually this is fine.  If there are no goals we'll do nothing. *)
      Not_found -> ()
  );

  let targets = List.rev !targets in

  (* Assignments are simply treated as statements added to the end of
   * the file (so they override earlier assignments to the same variable,
   * if any).
   *)
  let file = file @ List.rev !assignments in

  (* We start with an empty symbol table. *)
  let vars = Hashtbl.create 13 in

  (* Evaluate the target expressions in turn. *)
  Eval.evaluate file vars targets

let () = main ()
