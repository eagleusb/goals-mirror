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

type file = stmt list
and stmt =
  (* let id = expr *)
  | Let of id * expr
  (* goal id (params) = patterns : exprs = code *)
  | Goal of id * id list * pattern list * expr list * code option
and pattern =
  (* match tactic such as file ("filename") *)
  | PTactic of id * substs list
  (* match named variable, which must be a string or list *)
  | PVarSubst of id
and expr =
  (* goalname (params), tactic (params) etc. *)
  | ECall of id * expr list
  (* variable *)
  | EVar of id
  (* string with %-substitutions *)
  | EString of substs
  (* list *)
  | EList of expr list
and id = string
and code = substs
and substs = subst list
and subst =
  (* String literal part. *)
  | SString of string
  (* %-substitution. *)
  | SVar of id

(* This is used for incrementally building Ast.substs in the parser. *)
module Substs : sig
  type t
  val create : unit -> t
  val get : t -> substs
  val add_char : t -> char -> unit
  val add_string : t -> string -> unit
  val add_var : t -> string -> unit
end

val print_file : out_channel -> file -> unit
