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

module StringMap : sig
  type key = string
  type 'a t
  val empty: 'a t
  val add: key -> 'a -> 'a t -> 'a t
end

(** An environment is a set of variable and goal definitions, mapping
    variable or goal name -> expression. *)
type env = expr StringMap.t
and pattern =
  (** match tactic such as file ("filename") *)
  | PTactic of id * substs list
  (** match named variable, which must be a string or list *)
  | PVarSubst of id
and expr =
  (** goal (params) = patterns : exprs = code *)
  | EGoal of goal
  (** goalname (params), tactic (params) etc. *)
  | ECall of id * expr list
  (** variable *)
  | EVar of id
  (** list *)
  | EList of expr list
  (** string with %-substitutions *)
  | ESubsts of substs
  (** string with no substitutions *)
  | EString of string
and goal = id list * pattern list * expr list * code option
and id = string
and code = substs
and substs = subst list
and subst =
  (** String literal part. *)
  | SString of string
  (** %-substitution. *)
  | SVar of id

(** This is used for incrementally building Ast.substs in the parser. *)
module Substs : sig
  type t
  val create : unit -> t
  val get : t -> substs
  val add_char : t -> char -> unit
  val add_string : t -> string -> unit
  val add_var : t -> string -> unit
end

val print_env : out_channel -> env -> unit
