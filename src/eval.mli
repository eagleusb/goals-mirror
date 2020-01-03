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

(** Similar to {!substitute} except this is used where we will
    pass the result immediately to the shell to execute.  Variables
    are substituted with shell quoted strings.  Raises [Failure _]
    on error. *)
val to_shell_script : Ast.env -> Ast.loc -> Ast.substs -> string

(** Evaluate a goal argument.  This substitutes any variables found,
    and recursively calls functions. *)
val evaluate_goal_arg : Ast.env -> Ast.expr -> Ast.expr
