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
  | Ast.EGoal goal -> assert false

  (* This could be an instruction to call a goal, or it
   * could be a tactic.
   *)
  | Ast.ECall ("file", [filename]) (* XXX define tactics! *) ->
     (* All parameters of tactics must be simple expressions (strings,
      * in future booleans, numbers, etc).
      *)
     let args = [filename] in
     let args = List.map (simplify env) args in
     run_goal_for_tactic env "file" args

  | Ast.ECall ("file", _) ->
     failwith "file tactic called with wrong number of parameters"

  | Ast.ECall (name, args) ->
     let expr =
       try Ast.StringMap.find name env
       with Not_found -> failwithf "%s: goal not found" name in
     let goal =
       match expr with
       | Ast.EGoal goal -> goal
       | _ ->
          failwithf "%s: tried to call something which is not a goal" name in
     run_goal env name args goal

  (* Look up the variable and substitute it. *)
  | Ast.EVar name ->
     let expr =
       try Ast.StringMap.find name env
       with Not_found -> failwithf "%s: variable not found" name in
     evaluate_target env expr

  (* Lists are inlined when found as a target. *)
  | Ast.EList exprs ->
     evaluate_targets env exprs

  (* A string (with or without substitutions) implies file (filename). *)
  | Ast.ESubsts str ->
     let str = substitute env str in
     run_goal_for_tactic env "file" [Ast.CString str]

  | Ast.EConstant c ->
     run_goal_for_tactic env "file" [c]

(* Find the goal which matches the given tactic and run it.
 * Params is a list of constants.
 *)
and run_goal_for_tactic env tactic const_args =
  (* Search across all goals for a matching tactic. *)
  let goals =
    let env = Ast.StringMap.bindings env in
    filter_map
      (function (name, Ast.EGoal goal) -> Some (name, goal) | _ -> None)
      env in
  let name, goal =
    (* If there are multiple goals matching, this must choose
     * the most recently defined (XXX).
     *)
    try
      List.find
        (fun (_, (_, patterns, _, _)) ->
          List.exists (matching_pattern env tactic const_args) patterns)
        goals
    with
      Not_found ->
        failwithf "don't know how to build %s %s"
          tactic
          (String.concat ", "
             (List.map (function Ast.CString s -> s) const_args)) in

  let args = [] (* XXX calculate free variables *) in
  run_goal env name args goal

(* XXX This only does exact matches at the moment. *)
and matching_pattern env tactic const_args = function
  | PTactic (constructor, params)
       when tactic = constructor &&
            List.length const_args = List.length params ->
     (* Try to simplify the parameters of this pattern down
      * to constants, but don't fail here if we can't do this.
      *)
     (try
        let params = List.map (substitute env) params in
        let params = List.map (fun s -> Ast.CString s) params in
        const_args = params
      with Failure _ -> false
     )

  | PTactic _ -> false

  | PVar name -> assert false
(*
  NOT IMPLEMENTED - we need variables to contain constructors!
     (try
        let expr = Ast.StringMap.find name env in
        let expr = simplify env expr in
      with Not_found -> false
     )
*)

(* Run a named goal. *)
and run_goal env name args (params, patterns, deps, code) =
  (* Substitute the args for the parameters in the environment. *)
  let params =
    try List.combine params args
    with Invalid_argument _ ->
      failwithf "%s: calling goal with wrong number of arguments" name in
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
      let code = substitute env code in
      Printf.printf "running : %s\n" code
  );

  (* Check all targets were updated (else it's an error). *)
  (* XXX *)

(* Take any expression and simplify it down to a constant.
 * If the expression cannot be simplified then this throws
 * an error.
 *)
and simplify env = function
  | Ast.EConstant c -> c

  | Ast.EVar name ->
     let expr =
       try Ast.StringMap.find name env
       with Not_found -> failwithf "%s: variable not found" name in
     simplify env expr

  | Ast.ESubsts str ->
     Ast.CString (substitute env str)

  | Ast.EList _ ->
     failwith "list found where constant expression expected"

  | Ast.ECall (name, _) ->
     failwithf "%s: cannot use goal or tactic in constant expression" name

  | Ast.EGoal _ ->
     failwith "cannot use in constant expression"

(* Take a substitution list and try to turn it into a simple
 * string by evaluating every variable.  If not possible this
 * throws an error.  Returns a string.
 *)
and substitute env substs =
  let b = Buffer.create 13 in
  List.iter (
    function
    | Ast.SString s -> Buffer.add_string b s
    | Ast.SVar name ->
       let expr =
         try Ast.StringMap.find name env
         with Not_found -> failwithf "%s: variable not found" name in
       match simplify env expr with
       | Ast.CString s -> Buffer.add_string b s
  ) substs;
  Buffer.contents b
