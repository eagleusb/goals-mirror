(* Goalfile dependency DAG
 * Copyright (C) 2020 Richard W.M. Jones
 * Copyright (C) 2020 Red Hat Inc.
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

type t
(** The type of the directed acyclic graph of dependencies. *)

val create : Ast.env -> Ast.expr list -> t
(** [create env roots] constructs the DAG of dependencies starting
    with the roots, returning the final structure or raising
    [Failure _] if there is a problem. *)

type state
(** State held when running jobs. *)

type goal_runner =
  Ast.env -> Ast.loc -> string -> Ast.expr list -> Ast.goal ->
  Ast.expr list -> string -> unit
(** Run a single goal. *)

type exists_runner = Ast.env -> Ast.loc -> Ast.pattern -> string -> unit
(** Run an existence tactic. *)

val new_state : t -> goal_runner -> exists_runner -> state
(** Create a new state object from the DAG.

    [goal_runner] is a function for running single goals.
    [exists_runner] is a function for running existence tactics.
    See the {!Run} module. *)

type node

val retire_job : state -> node -> unit
val fail_job : state -> node -> unit
(** See {!Jobs.run}. *)

val next_job : state -> node Jobs.next
(** Returns the next job that is able to run.  (See {!Jobs.run}).

    It is always guaranteed that the next job which is returned
    has already had all its dependencies retired already. *)

val string_of_job : node -> string
