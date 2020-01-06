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

open Utils

module Env = struct
  include Map.Make (String)

  let merge env env' =
    List.fold_left (fun env (k, v) -> add k v env) env (bindings env')
end

type loc = position * position
let noloc = dummy_pos, dummy_pos

let string_loc () loc =
  let pos = fst loc in
  sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
let print_loc fp loc =
  fprintf fp "%s" (string_loc () loc)

type env = expr Env.t
and pattern =
  | PTactic of loc * id * substs list
and expr =
  | EGoalDefn of loc * goal
  | EFuncDefn of loc * func
  | ETacticDefn of loc * tactic
  | ECall of loc * id * expr list
  | ETacticCtor of loc * id * expr list
  | EVar of loc * id
  | EList of loc * expr list
  | ESubsts of loc * substs
  | EConstant of loc * constant
and constant =
  | CString of string
and goal = param_decl list * pattern list * expr list * code option
and func = param_decl list * returning * code
and tactic = param_decl list * code
and param_decl = id
and id = string
and code = substs
and returning = RetExpr | RetStrings | RetString
and substs = subst list
and subst =
  | SString of string
  | SVar of id

let getvar env loc name =
  try Env.find name env
  with Not_found ->
    failwithf "%a: variable ‘%s’ not found" string_loc loc name

let getgoal env loc name =
  let expr =
    try Env.find name env
    with Not_found ->
      failwithf "%a: goal ‘%s’ not found" string_loc loc name in
  let goal =
    match expr with
    | EGoalDefn (loc, goal) -> goal
    | _ ->
       failwithf "%a: tried to call ‘%s’ which is not a goal"
         string_loc loc name in
  goal

let getfunc env loc name =
  let expr =
    try Env.find name env
    with Not_found ->
      failwithf "%a: func ‘%s’ not found" string_loc loc name in
  let func =
    match expr with
    | EFuncDefn (loc, func) -> func
    | _ ->
       failwithf "%a: tried to call ‘%s’ which is not a function"
         string_loc loc name in
  func

let gettactic env loc name =
  assert (name.[0] = '*');
  let expr =
    try Env.find name env
    with Not_found ->
      failwithf "%a: tactic ‘%s’ not found" string_loc loc name in
  let tactic =
    match expr with
    | ETacticDefn (loc, tactic) -> tactic
    | _ ->
       failwithf "%a: tried to call ‘%s’ which is not a tactic"
         string_loc loc name in
  tactic

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

let rec string_env () env =
  let env = Env.bindings env in
  String.concat "" (List.map (string_def ()) env)

and print_env fp env = output_string fp (string_env () env)

and string_def () (name, expr) =
  match expr with
  | EGoalDefn (loc, goal) -> string_goal () (Some name, goal) ^ "\n"
  | EFuncDefn (loc, func) -> string_func () (Some name, func) ^ "\n"
  | ETacticDefn (loc, tactic) -> string_tactic () (Some name, tactic) ^ "\n"
  | expr -> sprintf "let %s = %a\n" name string_expr expr;

and print_def fp name expr = output_string fp (string_def () (name, expr))

and string_goal () (name, (param_decls, patterns, exprs, code)) =
  sprintf "goal%s (%s) = %s : %s%s"
    (match name with None -> "" | Some name -> " " ^ name)
    (String.concat ", " (List.map (string_param_decl ()) param_decls))
    (String.concat ", " (List.map (string_pattern ()) patterns))
    (String.concat ", " (List.map (string_expr ()) exprs))
    (match code with None -> "" | Some code -> " = { ... }")

and string_func () (name, (param_decls, returning, code)) =
  sprintf "function%s returning %s (%s) = { ... }"
    (match name with None -> "" | Some name -> " " ^ name)
    (match returning with RetExpr -> "expression"
                        | RetString -> "string"
                        | RetStrings -> "strings")
    (String.concat ", " (List.map (string_param_decl ()) param_decls))

and string_tactic () (name, (param_decls, code)) =
  sprintf "tactic%s (%s) = { ... }"
    (match name with None -> "" | Some name -> " " ^ name)
    (String.concat ", " (List.map (string_param_decl ()) param_decls))

and string_param_decl () name = name

and string_pattern () = function
  | PTactic (loc, name, params) ->
     sprintf "%s (%s)" name (String.concat ", "
                                (List.map (string_substs ()) params))

and print_pattern fp p = output_string fp (string_pattern () p)

and string_expr () = function
  | EGoalDefn (loc, goal) -> string_goal () (None, goal)
  | EFuncDefn (loc, func) -> string_func () (None, func)
  | ETacticDefn (loc, goal) -> string_tactic () (None, goal)
  | ECall (loc, name, params) ->
     sprintf "%s (%s)"
       name (String.concat ", " (List.map (string_expr ()) params))
  | ETacticCtor (loc, name, params) ->
     sprintf "%s (%s)"
       name (String.concat ", " (List.map (string_expr ()) params))
  | EVar (loc, var) -> var
  | EList (loc, xs) ->
     sprintf "[%s]" (String.concat ", " (List.map (string_expr ()) xs))
  | ESubsts (loc, s) -> string_substs () s
  | EConstant (loc, c) -> string_constant () c

and print_expr fp expr = output_string fp (string_expr () expr)

and string_constant () = function
  | CString s -> sprintf "%S" s

and print_constant fp c = output_string fp (string_constant () c)

and print_id = output_string

and string_substs () ss =
  let ss =
    List.map (
      function
      | SString s -> sprintf "%S" s
      | SVar id -> id
    ) ss in
  (String.concat "+" ss)

and print_substs fp ss = output_string fp (string_substs () ss)

and print_code fp xs =
  List.iter (
    function
    | SString s -> fprintf fp "%s" s
    | SVar id -> fprintf fp "%%%s" id
  ) xs
