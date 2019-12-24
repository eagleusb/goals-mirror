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

open Utils

let rec evaluate_targets env exprs =
  List.iter (evaluate_target env) exprs

and evaluate_target env = function
  | Ast.EGoal _ -> assert false

  (* Call a goal. *)
  | Ast.ECall (loc, name, args) ->
     let goal = Ast.getgoal env loc name in
     run_goal env loc name args goal

  | Ast.ETactic (loc, name, args) ->
     (* All parameters of tactics must be simple expressions (strings,
      * in future booleans, numbers, etc).
      *)
     let args = List.map (Ast.to_constant env) args in
     run_tactic env loc name args

  (* Look up the variable and substitute it. *)
  | Ast.EVar (loc, name) ->
     let expr = Ast.getvar env loc name in
     evaluate_target env expr

  (* Lists are inlined when found as a target. *)
  | Ast.EList (loc, exprs) ->
     evaluate_targets env exprs

  (* A string (with or without substitutions) implies *file(filename). *)
  | Ast.ESubsts (loc, str) ->
     let str = Ast.substitute env loc str in
     run_tactic env loc "file" [Ast.CString str]

  | Ast.EConstant (loc, c) ->
     run_tactic env loc "file" [c]

(* Run a goal by name. *)
and run_goal env loc name args (params, patterns, deps, code) =
  (* Create a new environment which maps the parameter names to
   * the args.
   *)
  let env =
    let params =
      try List.combine params args
      with Invalid_argument _ ->
        failwithf "%a: calling goal ‘%s’ with wrong number of arguments"
          Ast.string_loc loc name in
    List.fold_left (fun env (k, v) -> Ast.Env.add k v env) env params in

  (* Check all dependencies have been updated. *)
  evaluate_targets env deps;

  (* Check if any target (ie. pattern) needs to be rebuilt. *)
  let rebuild =
    List.exists (needs_rebuild env loc name deps) patterns in

  if rebuild then (
    (* Run the code (if any). *)
    (match code with
     | None -> ()
     | Some code ->
        let code = Ast.substitute env loc code in
        Printf.printf "running : %s\n" code
    );

    (* Check all targets were updated (else it's an error). *)
    let pattern_still_needs_rebuild =
      try Some (List.find (needs_rebuild env loc name deps) patterns)
      with Not_found -> None in
    match pattern_still_needs_rebuild with
    | None -> ()
    | Some pattern ->
       failwithf "%a: goal ‘%s’ ran successfully but it did not rebuild %a"
         Ast.string_loc loc
         name
         Ast.string_pattern pattern
  )

(* Return whether the target (pattern) needs to be rebuilt. *)
and needs_rebuild env loc name deps pattern =
  false (* XXX *)

(* Find the goal which matches the given tactic and run it.
 * const_args is a list of parameters (all constants).
 *)
and run_tactic env loc tactic const_args =
  (* Search across all goals for a matching tactic. *)
  let goals =
    let env = Ast.Env.bindings env in
    filter_map
      (function (name, Ast.EGoal (loc, goal)) -> Some (name, goal) | _ -> None)
      env in
  let name, goal =
    (* If there are multiple goals matching, this must choose
     * the most recently defined (XXX).
     *)
    try
      List.find
        (fun (_, (_, patterns, _, _)) ->
          List.exists (matching_pattern env loc tactic const_args) patterns)
        goals
    with
      Not_found ->
        let tactic =
          Ast.ETactic (loc, tactic,
                       List.map (fun c -> Ast.EConstant (loc, c))
                         const_args) in
        failwithf "%a: don't know how to build %a"
          Ast.string_loc loc Ast.string_expr tactic in

  let args = [] (* XXX calculate free variables *) in
  run_goal env loc name args goal

(* XXX This only does exact matches at the moment. *)
and matching_pattern env loc tactic const_args = function
  | Ast.PTactic (loc, constructor, params)
       when tactic = constructor &&
            List.length const_args = List.length params ->
     (* Try to simplify the parameters of this pattern down
      * to constants, but don't fail here if we can't do this.
      *)
     (try
        let params = List.map (Ast.substitute env loc) params in
        let params = List.map (fun s -> Ast.CString s) params in
        const_args = params
      with Failure _ -> false
     )

  | Ast.PTactic _ -> false

  | Ast.PVar (loc, name) -> assert false (* not implemented *)

