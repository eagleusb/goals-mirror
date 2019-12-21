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

let usage =
  "\
goals: Build software.

 goals [-f Goalfile] ['var = value' ...] [target ...]

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
  let file = Parse.parse_from_file filename in

  Ast.print_file stdout file

let () = main ()
