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

let usage =
  "\
goals: Build software.

 goals [-f Goalfile] ['var = value' ...] ['target' ...]

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

  Ast.print_file stdout file;

  (* Find the target(s) to execute first. *)
  let initial_targets = ref [] in
  (* XXX Parse command line anon args here. XXX *)

  (* If no initial target set on the command line, find
   * the first goal in the file.
   *)
  List.iter (
    function
    | Ast.Goal (name, [], _, _, _) ->
       if !initial_targets = [] then
         initial_targets := Ast.ECall (name, []) :: !initial_targets
    | Ast.Goal (name, _, _, _, _) ->
       if !initial_targets = [] then
         failwithf "%s: first target ‘%s’ has parameters and so cannot be used as the default target"
           filename name
    | _ -> ()
  ) file;

  let initial_targets = List.rev !initial_targets in
  ignore initial_targets

let () = main ()
