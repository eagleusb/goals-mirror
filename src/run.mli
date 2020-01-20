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

val goal_runner : Deps.goal_runner
(** Run a single goal. *)

val exists_runner : Deps.exists_runner
(** Run the implicit existence predicate.

    This is used when we find a predicate like is-foo(...) but there
    is no matching goal.  We run (in this callback) the associated
    predicate code.  As long as it runs successfully, not returning 99 or
    any error value, then we're OK - the predicate doesn't need rebuilding
    so the dependency is satisfied.  However if it returns 99 (needs
    rebuild) or an error then we have to exit with an error. *)
