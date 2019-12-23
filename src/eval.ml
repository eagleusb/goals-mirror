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
     let expr =
       try Ast.StringMap.find name env
       with Not_found ->
         failwithf "%a: goal ‘%s’ not found" Ast.string_loc loc name in
     let goal =
       match expr with
       | Ast.EGoal (loc, goal) -> goal
       | _ ->
          failwithf "%a: tried to call ‘%s’ which is not a goal"
            Ast.string_loc loc name in
     run_goal loc env name args goal

  | Ast.ETactic (loc, name, args) ->
     (* All parameters of tactics must be simple expressions (strings,
      * in future booleans, numbers, etc).
      *)
     let args = List.map (simplify env) args in
     run_goal_for_tactic loc env name args

  (* Look up the variable and substitute it. *)
  | Ast.EVar (loc, name) ->
     let expr =
       try Ast.StringMap.find name env
       with Not_found ->
         failwithf "%a: variable ‘%s’ not found" Ast.string_loc loc name in
     evaluate_target env expr

  (* Lists are inlined when found as a target. *)
  | Ast.EList (loc, exprs) ->
     evaluate_targets env exprs

  (* A string (with or without substitutions) implies *file(filename). *)
  | Ast.ESubsts (loc, str) ->
     let str = substitute loc env str in
     run_goal_for_tactic loc env "file" [Ast.CString str]

  | Ast.EConstant (loc, c) ->
     run_goal_for_tactic loc env "file" [c]

(* Find the goal which matches the given tactic and run it.
 * Params is a list of constants.
 *)
and run_goal_for_tactic loc env tactic const_args =
  (* Search across all goals for a matching tactic. *)
  let goals =
    let env = Ast.StringMap.bindings env in
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
          List.exists (matching_pattern loc env tactic const_args) patterns)
        goals
    with
      Not_found ->
        failwithf "%a: don't know how to build %s %s"
          Ast.string_loc loc
          tactic
          (String.concat ", "
             (List.map (function Ast.CString s -> s) const_args)) in

  let args = [] (* XXX calculate free variables *) in
  run_goal loc env name args goal

(* XXX This only does exact matches at the moment. *)
and matching_pattern loc env tactic const_args = function
  | Ast.PTactic (loc, constructor, params)
       when tactic = constructor &&
            List.length const_args = List.length params ->
     (* Try to simplify the parameters of this pattern down
      * to constants, but don't fail here if we can't do this.
      *)
     (try
        let params = List.map (substitute loc env) params in
        let params = List.map (fun s -> Ast.CString s) params in
        const_args = params
      with Failure _ -> false
     )

  | Ast.PTactic _ -> false

  | Ast.PVar (loc, name) -> assert false
(*
  NOT IMPLEMENTED - we need variables to contain constructors!
     (try
        let expr = Ast.StringMap.find name env in
        let expr = simplify env expr in
      with Not_found -> false
     )
*)

(* Run a named goal. *)
and run_goal loc env name args (params, patterns, deps, code) =
  (* Substitute the args for the parameters in the environment. *)
  let params =
    try List.combine params args
    with Invalid_argument _ ->
      failwithf "%a: calling goal ‘%s’ with wrong number of arguments"
        Ast.string_loc loc name in
  let env =
    List.fold_left (fun env (k, v) -> Ast.StringMap.add k v env)
      env params in

  (* Evaluate the dependencies first. *)
  evaluate_targets env deps;

  (* Check if any target needs to be updated. *)
  (* XXX *)

  (* Run the code (if any). *)
  (match code with
   | None -> ()
   | Some code ->
      let code = substitute loc env code in
      Printf.printf "running : %s\n" code
  );

  (* Check all targets were updated (else it's an error). *)
  (* XXX *)

(* Take any expression and simplify it down to a constant.
 * If the expression cannot be simplified then this throws
 * an error.
 *)
and simplify env = function
  | Ast.EConstant (loc, c) -> c

  | Ast.EVar (loc, name) ->
     let expr =
       try Ast.StringMap.find name env
       with Not_found ->
         failwithf "%a: variable ‘%s’ not found" Ast.string_loc loc name in
     simplify env expr

  | Ast.ESubsts (loc, str) ->
     Ast.CString (substitute loc env str)

  | Ast.EList (loc, _) ->
     failwithf "%a: list found where constant expression expected"
       Ast.string_loc loc

  | Ast.ECall (loc, name, _) ->
     failwithf "%a: cannot use goal ‘%s’ in constant expression"
       Ast.string_loc loc name

  | Ast.ETactic (loc, name, _) ->
     failwithf "%a: cannot use tactic ‘*%s’ in constant expression"
       Ast.string_loc loc name

  | Ast.EGoal (loc, _) ->
     failwithf "%a: cannot use goal in constant expression"
       Ast.string_loc loc

(* Take a substitution list and try to turn it into a simple
 * string by evaluating every variable.  If not possible this
 * throws an error.  Returns a string.
 *)
and substitute loc env substs =
  let b = Buffer.create 13 in
  List.iter (
    function
    | Ast.SString s -> Buffer.add_string b s
    | Ast.SVar name ->
       let expr =
         try Ast.StringMap.find name env
         with Not_found ->
           failwithf "%a: variable ‘%s’ not found" Ast.string_loc loc name in
       match simplify env expr with
       | Ast.CString s -> Buffer.add_string b s
  ) substs;
  Buffer.contents b
