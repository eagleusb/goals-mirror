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

open Lexing
open Printf

module StringMap = Map.Make (String)

type loc = position * position
let noloc = dummy_pos, dummy_pos

let string_loc () loc =
  let pos = fst loc in
  sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
let print_loc fp loc =
  fprintf fp "%s" (string_loc () loc)

type env = expr StringMap.t
and pattern =
  | PTactic of loc * id * substs list
  | PVar of loc * id
and expr =
  | EGoal of loc * goal
  | ECall of loc * id * expr list
  | EVar of loc * id
  | EList of loc * expr list
  | ESubsts of loc * substs
  | EConstant of loc * constant
and constant =
  | CString of string
and goal = id list * pattern list * expr list * code option
and id = string
and code = substs
and substs = subst list
and subst =
  | SString of string
  | SVar of id

module Substs = struct
  type t = {
      mutable elems : subst list; (* built in reverse order *)
      curr : Buffer.t;            (* current string *)
    }

  let create () = { elems = []; curr = Buffer.create 13 }

  let finalize t =
    if Buffer.length t.curr > 0 then
      t.elems <- SString (Buffer.contents t.curr) :: t.elems;
    Buffer.clear t.curr

  let get t = finalize t; List.rev t.elems

  let add_char { curr } = Buffer.add_char curr
  let add_string { curr} = Buffer.add_string curr
  let add_var t id = finalize t; t.elems <- SVar id :: t.elems
end

let iter_with_commas
    : out_channel -> (out_channel -> 'a -> unit) -> 'a list -> unit =
  fun fp f xs ->
  let comma = ref false in
  List.iter (
    fun x ->
      if !comma then fprintf fp ", ";
      comma := true;
      f fp x
  ) xs

let rec print_env fp env =
  StringMap.iter (print_def fp) env

and print_def fp name expr =
  match expr with
  | EGoal (loc, (params, patterns, exprs, code)) ->
     fprintf fp "goal %s (%s) =\n" name (String.concat ", " params);
     fprintf fp "    ";
     iter_with_commas fp print_pattern patterns;
     fprintf fp " : ";
     iter_with_commas fp print_expr exprs;
     (match code with
      | None -> ()
      | Some code ->
         fprintf fp " {\n";
         print_code fp code;
         fprintf fp "\n    }"
     );
     fprintf fp "\n"
  | expr ->
     fprintf fp "let %s = " name;
     print_expr fp expr;
     fprintf fp "\n"

and print_pattern fp = function
  | PTactic (loc, name, params) ->
     fprintf fp "%s (" name;
     iter_with_commas fp print_substs params;
     fprintf fp ")"
  | PVar (loc, id) -> print_id fp id

and print_expr fp = function
  | EGoal _ -> assert false (* printed above *)
  | ECall (loc, name, params) ->
     fprintf fp "%s (" name;
     iter_with_commas fp print_expr params;
     fprintf fp ")"
  | EVar (loc, var) -> print_id fp var
  | EList (loc, xs) ->
     fprintf fp "[";
     iter_with_commas fp print_expr xs;
     fprintf fp "]"
  | ESubsts (loc, s) -> print_substs fp s
  | EConstant (loc, c) -> print_constant fp c

and print_constant fp = function
  | CString s -> fprintf fp "%S" s

and print_id = output_string

and print_substs fp xs =
  let xs =
    List.map (
      function
      | SString s -> sprintf "%S" s
      | SVar id -> id
    ) xs in
  fprintf fp "%s" (String.concat "+" xs)

and print_code fp xs =
  List.iter (
    function
    | SString s -> fprintf fp "%s" s
    | SVar id -> fprintf fp "%%%s" id
  ) xs
