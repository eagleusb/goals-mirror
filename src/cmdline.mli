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

val parse : unit -> (string * string) list * string list
(** Parse the command line.
    Returns two lists:
    anon_vars = List of anonymous variable assignments.
    targets = List of target expressions on the command line. *)

val stdlibdir : string
(** Get the stdlib directory. *)

val prelude_gl_file : string
(** Get the absolute path of the prelude.gl file. *)

val prelude_sh_file : string
(** Get the absolute path of the prelude.sh file. *)

val input_file : unit -> string
(** Get the name of the input Goalfile. *)

val debug : ('a, unit, string, unit) format4 -> 'a
(** If debugging is enabled (-d option) then print the formatted
    output.  If debugging was not enabled then nothing is printed. *)

val debug_flag : unit -> bool
(** If debugging is enabled. *)

val directory : unit -> string
(** Get the name of working directory (-C option). *)

val includes : unit -> string list
(** Get list of include directories (-I option). *)

val nr_jobs : unit -> int
(** Number of jobs (-j option). *)

val use_prelude : unit -> bool
(** True if we should load the prelude, or false if --no-prelude. *)
