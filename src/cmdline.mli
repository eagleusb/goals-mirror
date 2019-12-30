(* Goals command line
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

val stdlibdir : string
(** Get the stdlib directory. *)

val prelude_file : string
(** Get the absolute path of the prelude.gl file. *)

val input_file : string
(** Get the name of the input Goalfile.
    This is an absolute path. *)

val debug : ('a, unit, string, unit) format4 -> 'a
(** If debugging is enabled (-d option) then print the formatted
    output.  If debugging was not enabled then nothing is printed. *)

val debug_flag : bool
(** If debugging is enabled. *)

val directory : string
(** Get the name of working directory (-C option). *)

val includes : string list
(** Get list of include directories (-I option).
    These are all absolute paths. *)

val use_prelude : bool
(** True if we should load the prelude, or false if --no-prelude. *)

val anon_vars : (string * string) list
(** List of anonymous variable assignments on the command line. *)

val targets : string list
(** List of target expressions on the command line. *)
