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

  (* This is not the normal Map.merge function. *)
  val merge : 'a t -> 'a t -> 'a t
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
  | PTactic of loc * id * substs list
  (** match tactic such as *file ("filename") *)
and expr =
  | EGoalDefn of loc * goal
  (** goal (params) = patterns : exprs code *)
  | EFuncDefn of loc * func
  (** function (params) = code *)
  | ETacticDefn of loc * tactic
  (** tactic (params) = code *)
  | ECall of loc * id * expr list
  (** call goal (params) or function (params) *)
  | ETacticCtor of loc * id * expr list
  (** constructor *tactic (params) *)
  | EVar of loc * id
  (** variable, or goal call with no parameters *)
  | EList of loc * expr list
  (** list *)
  | ESubsts of loc * substs
  (** string with %-substitutions *)
  | EConstant of loc * constant
  (** constant expression, such as a plain string, int, boolean, etc. *)
and constant =
  | CString of string
and goal = param_decl list * pattern list * expr list * code option
and func = param_decl list * returning * code
and tactic = param_decl list * code
and param_decl = id             (** goal/func/tactic parameter. *)
and id = string
and code = substs * bool        (** code + quiet flag *)
and returning = RetExpr | RetStrings | RetString
and substs = subst list
and subst =
  | SString of string
  (** String literal part. *)
  | SVar of id
  (** %-substitution. *)

val getvar : env -> loc -> id -> expr
(** Look up a variable in the environment.  Raise [Failure _]
    if not found. *)

val getgoal : env -> loc -> id -> goal
(** Look up a goal in the environment.  Raise [Failure _] if not
    found or if the named variable is not a goal. *)

val getfunc : env -> loc -> id -> func
(** Look up a function in the environment.  Raise [Failure _] if not
    found or if the named variable is not a function. *)

val gettactic : env -> loc -> id -> tactic
(** Look up a tactic in the environment.  Raise [Failure _] if not
    found or if the named variable is not a tactic. *)

(** This is used for incrementally building Ast.substs in the parser. *)
module Substs : sig
  type t
  val create : unit -> t
  val get : t -> substs
  val add_char : t -> char -> unit
  val add_string : t -> string -> unit
  val add_var : t -> string -> unit
end

(** %a formatters. *)
val print_env : out_channel -> env -> unit
val string_pattern : unit -> pattern -> string
val string_expr : unit -> expr -> string
val print_expr : out_channel -> expr -> unit
