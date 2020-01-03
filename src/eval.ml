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

open Utils

let rec to_constant env = function
  | Ast.EConstant (loc, c) -> c

  | EVar (loc, name) ->
     let expr = Ast.getvar env loc name in
     to_constant env expr

  | ESubsts (loc, str) ->
     CString (substitute env loc str)

  | EList (loc, _) ->
     failwithf "%a: list found where constant expression expected"
       Ast.string_loc loc

  | ECallGoal (loc, name, _) ->
     failwithf "%a: cannot use goal ‘%s’ in constant expression"
       Ast.string_loc loc name

  | ETacticCtor (loc, name, _) ->
     failwithf "%a: cannot use tactic ‘%s’ in constant expression"
       Ast.string_loc loc name

  | EGoalDefn (loc, _) ->
     failwithf "%a: cannot use goal in constant expression"
       Ast.string_loc loc

  | ETacticDefn (loc, _) ->
     failwithf "%a: cannot use tactic in constant expression"
       Ast.string_loc loc

and substitute env loc substs =
  let b = Buffer.create 13 in
  List.iter (
    function
    | Ast.SString s -> Buffer.add_string b s
    | SVar name ->
       let expr = Ast.getvar env loc name in
       match to_constant env expr with
       | Ast.CString s -> Buffer.add_string b s
  ) substs;
  Buffer.contents b

let rec to_shell_script env loc substs =
  let b = Buffer.create 13 in
  List.iter (
    function
    | Ast.SString s -> Buffer.add_string b s
    | SVar name ->
       let expr = Ast.getvar env loc name in
       let s = expr_to_shell_string env expr in
       Buffer.add_string b s
  ) substs;
  Buffer.contents b

and expr_to_shell_string env = function
  | Ast.EConstant (loc, CString s) -> Filename.quote s

  | EVar (loc, name) ->
     let expr = Ast.getvar env loc name in
     expr_to_shell_string env expr

  | ESubsts (loc, str) ->
     Filename.quote (substitute env loc str)

  | EList (loc, exprs) ->
     let strs = List.map (expr_to_shell_string env) exprs in
     (* These are shell quoted so we can just concat them with space. *)
     String.concat " " strs

  | ECallGoal (loc, name, _) ->
     failwithf "%a: cannot use goal ‘%s’ in shell expansion"
       Ast.string_loc loc name

  (* Tactics expand to the first parameter. *)
  | ETacticCtor (loc, _, []) -> Filename.quote ""
  | ETacticCtor (loc, _, (arg :: _)) -> expr_to_shell_string env arg

  | EGoalDefn (loc, _) ->
     failwithf "%a: cannot use goal in shell expansion"
       Ast.string_loc loc

  | ETacticDefn (loc, _) ->
     failwithf "%a: cannot use tactic in shell expansion"
       Ast.string_loc loc

let rec evaluate_goal_arg env = function
  | Ast.EVar (loc, name) ->
     let expr = Ast.getvar env loc name in
     evaluate_goal_arg env expr

  | ESubsts (loc, str) ->
     let str = Ast.substitute env loc str in
     Ast.EConstant (loc, Ast.CString str)

  | EList (loc, exprs) ->
     Ast.EList (loc, List.map (evaluate_goal_arg env) exprs)

  | ETacticCtor (loc, name, exprs) ->
     Ast.ETacticCtor (loc, name, List.map (evaluate_goal_arg env) exprs)

  | ECallGoal (loc, name, _) ->
     (* Goals don't return anything so they cannot be used in
      * goal args.  Use a function instead.
      *)
     failwithf "%a: cannot use goal ‘%s’ in goal argument"
       Ast.string_loc loc name

  | EConstant _
  | EGoalDefn _
  | ETacticDefn _ as e -> e
