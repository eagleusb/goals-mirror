(* Goalfile Abstract Syntax Tree
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

module Env : sig
  type key = string
  type 'a t
  val empty: 'a t
  val add: key -> 'a -> 'a t -> 'a t
  val find: key -> 'a t -> 'a
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val filter: (key -> 'a -> bool) -> 'a t -> 'a t
  val bindings: 'a t -> (key * 'a) list
end

(** Location where we parsed from $loc = $startpos, $endpos *)
type loc = Lexing.position * Lexing.position
val noloc : loc
val print_loc : out_channel -> loc -> unit
val string_loc : unit -> loc -> string

(** An environment is a set of variable and goal definitions, mapping
    variable or goal name -> expression. *)
type env = expr Env.t
and pattern =
  (** match tactic such as *file ("filename") *)
  | PTactic of loc * id * substs list
and expr =
  (** goal (params) = patterns : exprs code *)
  | EGoal of loc * goal
  (** tactic (params) = code *)
  | ETactic of loc * tactic
  (** call goalname (params) etc. *)
  | ECallGoal of loc * id * expr list
  (** call *tactic (params) etc. *)
  | ECallTactic of loc * id * expr list
  (** variable, or goal call with no parameters *)
  | EVar of loc * id
  (** list *)
  | EList of loc * expr list
  (** string with %-substitutions *)
  | ESubsts of loc * substs
  (** constant expression, such as a plain string, int, boolean, etc. *)
  | EConstant of loc * constant
and constant =
  | CString of string
and goal = param_decl list * pattern list * expr list * code option
and tactic = param_decl list * code
  (** Goal/tactic parameter. *)
and param_decl = id
and id = string
and code = substs
and substs = subst list
and subst =
  (** String literal part. *)
  | SString of string
  (** %-substitution. *)
  | SVar of id

(** Look up a variable in the environment.  Raise [Failure _]
    if not found. *)
val getvar : env -> loc -> id -> expr

(** Look up a goal in the environment.  Raise [Failure _] if not
    found or if the named variable is not a goal. *)
val getgoal : env -> loc -> id -> goal

(** Look up a tactic in the environment.  Raise [Failure _] if not
    found or if the named variable is not a tactic. *)
val gettactic : env -> loc -> id -> tactic

(** Take any expression and simplify it down to a constant.
    If the expression cannot be simplified then this raises
    [Failure _]. *)
val to_constant : env -> expr -> constant

(** Take a substitution list and try to turn it into a simple
    string by evaluating every variable.  If not possible this
    raises [Failure _]. *)
val substitute : env -> loc -> substs -> string

(** Similar to {!substitute} except this is used where we will
    pass the result immediately to the shell to execute.  Variables
    are substituted with shell quoted strings.  Raises [Failure _]
    on error. *)
val to_shell_script : env -> loc -> substs -> string

(** This is used for incrementally building Ast.substs in the parser. *)
module Substs : sig
  type t
  val create : unit -> t
  val get : t -> substs
  val add_char : t -> char -> unit
  val add_string : t -> string -> unit
  val add_var : t -> string -> unit
end

(** Print all definitions in an environment. *)
val print_env : out_channel -> env -> unit

(** %a formatters. *)
val string_pattern : unit -> pattern -> string
val string_expr : unit -> expr -> string
