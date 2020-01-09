(* Goalfile evaluation
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

val run_targets_to_completion : Ast.env -> Ast.expr list -> unit
(** This drives evaluation of the list of target expressions (in
    parallel) until they are complete or we reach an error.  The
    expressions are either a list of dependencies and/or a list of
    initial targets. *)

val stop_all : unit -> unit
(** Wait until all running jobs finish, and don't start any new ones.
    See [Jobs.stop_all]. *)
