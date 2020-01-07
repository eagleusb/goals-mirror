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

(** Take any expression and simplify it down to a constant.
    If the expression cannot be simplified then this raises
    [Failure _]. *)
val to_constant : Ast.env -> Ast.expr -> Ast.constant

(** Take a substitution list and try to turn it into a simple
    string by evaluating every variable.  If not possible this
    raises [Failure _]. *)
val substitute : Ast.env -> Ast.loc -> Ast.substs -> string

(** Run a code section.  Returns the exit code.
    Raises [Failure _] on error. *)
val run_code : Ast.env -> Ast.loc -> Ast.code -> int

(** Run a code section.  Returns the exit code and the full stdout
    as a string.  Raises [Failure _] on error. *)
val run_code_to_string : Ast.env -> Ast.loc -> Ast.code -> int * string

(** Run a code section.  Returns the exit code and the full stdout
    as a list of strings.  Raises [Failure _] on error. *)
val run_code_to_string_list : Ast.env -> Ast.loc -> Ast.code ->
                              int * string list

(** Evaluate a goal argument.  This substitutes any variables found,
    and recursively calls functions. *)
val evaluate_goal_arg : Ast.env -> Ast.expr -> Ast.expr

(* Call a function. *)
val call_function : Ast.env -> Ast.loc -> Ast.id -> Ast.expr list -> Ast.func ->
                    Ast.expr
