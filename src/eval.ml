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

open Printf

open Utils

let rec evaluate_targets env exprs =
  List.iter (evaluate_target env) exprs

and evaluate_target env = function
  | Ast.EGoalDefn _ | Ast.ETacticDefn _ -> assert false

  (* Call a goal. *)
  | Ast.ECallGoal (loc, name, args) ->
     let goal = Ast.getgoal env loc name in
     run_goal env loc name args goal

  (* Call a tactic. *)
  | Ast.ETacticConstructor (loc, name, args) ->
     (* All parameters of tactics must be simple constant expressions
      * (strings, in future booleans, numbers, etc).
      *)
     let args = List.map (Ast.to_constant env) args in
     run_tactic env loc name args

  (* If this is a goal then it's the same as calling goal().  If not
   * then look up the variable and substitute it.
   *)
  | Ast.EVar (loc, name) ->
     let expr = Ast.getvar env loc name in
     (match expr with
      | EGoalDefn (loc, ([], _, _, _)) ->
         evaluate_target env (Ast.ECallGoal (loc, name, []))
      | EGoalDefn _ ->
         failwithf "%a: cannot call %s() since this goal has parameters"
           Ast.string_loc loc name
      | _ ->
         evaluate_target env expr
     )

  (* Lists are inlined when found as a target. *)
  | Ast.EList (loc, exprs) ->
     evaluate_targets env exprs

  (* A string (with or without substitutions) implies *file(filename). *)
  | Ast.ESubsts (loc, str) ->
     let str = Ast.substitute env loc str in
     run_tactic env loc "*file" [Ast.CString str]

  | Ast.EConstant (loc, c) ->
     run_tactic env loc "*file" [c]

(* Run a goal by name. *)
and run_goal env loc name args (params, patterns, deps, code) =
  Cmdline.debug "%a: running goal %s %a"
    Ast.string_loc loc name Ast.string_expr (Ast.EList (Ast.noloc, args));

  (* This is the point where we evaluate the goal arguments.  We must
   * do this before creating the new environment, because variables
   * appearing in goal arguments don't refer to goal parameters.
   *)
  let args = List.map (evaluate_goal_arg env) args in

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

  (* Check if any target (ie. pattern) needs to be rebuilt.
   * As with make, a goal with no targets is always run.
   *)
  let rebuild =
    patterns = [] || List.exists (needs_rebuild env loc deps) patterns in

  if rebuild then (
    (* Run the code (if any). *)
    (match code with
     | None -> ()
     | Some code ->
        (* Add some standard variables to the environment. *)
        let expr_of_substs s = Ast.ESubsts (Ast.noloc, s) in
        let expr_of_pattern = function
          | Ast.PTactic (loc, tactic, targs) ->
             Ast.ETacticConstructor (loc, tactic, List.map expr_of_substs targs)
        in
        let pexprs = List.map expr_of_pattern patterns in
        let env = Ast.Env.add "@" (Ast.EList (Ast.noloc, pexprs)) env in
        let env = Ast.Env.add "<" (Ast.EList (Ast.noloc, deps)) env in
        let env =
          match deps with
          | [] -> env
          | d :: _ -> Ast.Env.add "^" d env in
        let code = Ast.to_shell_script env loc code in
        let code = "set -e\nset -x\n\n" ^ code in
        let r = Sys.command code in
        if r <> 0 then (
          eprintf "*** goal ‘%s’ failed with exit code %d\n" name r;
          exit 1
        )
    );

    (* Check all targets were updated (else it's an error). *)
    let pattern_still_needs_rebuild =
      try Some (List.find (needs_rebuild env loc deps) patterns)
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
and needs_rebuild env loc deps pattern =
  Cmdline.debug "%a: testing if %a needs rebuild"
    Ast.string_loc loc Ast.string_pattern pattern;

  match pattern with
  | Ast.PTactic (loc, tactic, targs) ->
     (* Look up the tactic. *)
     let params, code = Ast.gettactic env loc tactic in

     (* Resolve the targs down to constants.  Since needs_rebuild
      * should be called with env containing the goal params, this
      * should substitute any parameters in the tactic arguments.
      *)
     let targs = List.map (Ast.substitute env loc) targs in
     let targs =
       List.map (fun targ ->
           Ast.EConstant (Ast.noloc, Ast.CString targ)) targs in

     (* Create a new environment binding parameter names
      * to tactic args.
      *)
     let env =
       let params =
         try List.combine params targs
         with Invalid_argument _ ->
           failwithf "%a: calling tactic ‘%s’ with wrong number of arguments"
             Ast.string_loc loc tactic in
       List.fold_left (fun env (k, v) -> Ast.Env.add k v env) env params in

     (* Add some standard variables to the environment. *)
     let env = Ast.Env.add "<" (Ast.EList (Ast.noloc, deps)) env in
     let env =
       match deps with
       | [] -> env
       | d :: _ -> Ast.Env.add "^" d env in
     let code = Ast.to_shell_script env loc code in
     let code = "set -e\nset -x\n\n" ^ code in
     let r = Sys.command code in
     if r = 99 (* means "needs rebuild" *) then true
     else if r = 0 (* means "doesn't need rebuild" *) then false
     else (
       eprintf "*** tactic ‘%s’ failed with exit code %d\n" tactic r;
       exit 1
     )

(* Evaluate a goal argument.  This substitutes any variables found,
 * and recursively calls functions.
 *)
and evaluate_goal_arg env = function
  | Ast.EVar (loc, name) ->
     let expr = Ast.getvar env loc name in
     evaluate_goal_arg env expr

  | Ast.ESubsts (loc, str) ->
     let str = Ast.substitute env loc str in
     Ast.EConstant (loc, Ast.CString str)

  | Ast.EList (loc, exprs) ->
     Ast.EList (loc, List.map (evaluate_goal_arg env) exprs)

  | Ast.ETacticConstructor (loc, name, exprs) ->
     Ast.ETacticConstructor (loc, name, List.map (evaluate_goal_arg env) exprs)

  | Ast.ECallGoal (loc, name, _) ->
     (* Goals don't return anything so they cannot be used in
      * goal args.  Use a function instead.
      *)
     failwithf "%a: cannot use goal ‘%s’ in goal argument"
       Ast.string_loc loc name

  | Ast.EConstant _
  | Ast.EGoalDefn _
  | Ast.ETacticDefn _ as e -> e

(* Find the goal which matches the given tactic and run it.
 * cargs is a list of parameters (all constants).
 *)
and run_tactic env loc tactic cargs =
  Cmdline.debug "%a: running tactic %s" Ast.string_loc loc tactic;

  (* Find all goals in the environment.  Returns a list of (name, goal). *)
  let goals =
    let env = Ast.Env.bindings env in
    filter_map
      (function
       | name, Ast.EGoalDefn (loc, goal) -> Some (name, goal)
       | _ -> None) env in

  (* Find all patterns.  Returns a list of (pattern, name, goal). *)
  let patterns : (Ast.pattern * Ast.id * Ast.goal) list =
    List.flatten
      (List.map (fun (name, ((_, patterns, _, _) as goal)) ->
           List.map (fun pattern -> (pattern, name, goal)) patterns) goals) in

  (* Find any patterns (ie. tactics) which match the one we
   * are searching for.  This returns a binding for the goal args,
   * so we end up with a list of (pattern, name, goal, args).
   *)
  let patterns : (Ast.pattern * Ast.id * Ast.goal * Ast.expr list) list =
    filter_map (
      fun (pattern, name, ((params, _, _, _) as goal)) ->
        match matching_pattern env loc tactic cargs pattern params with
        | None -> None
        | Some args -> Some (pattern, name, goal, args)
    ) patterns in

  match patterns with
  | [] ->
     (* There's no matching goal, but we don't need one if
      * the tactic doesn't need to be rebuilt.
      *)
     let targs = List.map (function Ast.CString s -> [Ast.SString s]) cargs in
     let p = Ast.PTactic (loc, tactic, targs) in
     if needs_rebuild env loc [] p then (
       let t = Ast.ETacticConstructor (loc, tactic,
                                List.map (fun c -> Ast.EConstant (loc, c))
                                  cargs) in
       failwithf "%a: don't know how to build %a"
         Ast.string_loc loc Ast.string_expr t
     )

  | goals ->
     (* One or more goals match.  We run them all (although if
      * one of them succeeds in rebuilding, it will cut short the rest).
      *)
     List.iter (
       fun (_, name, goal, args) ->
         run_goal env loc name args goal
     ) goals

(* Test if pattern matches *tactic(cargs).  If it does
 * then we return Some args where args is the arguments that must
 * be passed to the matching goal.  The params parameter is
 * the names of the parameters of that goal.
 *)
and matching_pattern env loc tactic cargs pattern params =
  match pattern with
  | Ast.PTactic (loc, ttactic, targs)
       when tactic <> ttactic ||
            List.length cargs <> List.length targs ->
     None (* Can't possibly match if tactic name or #args is different. *)
  | Ast.PTactic (loc, ttactic, targs) ->
     (* Do the args match with a possible params binding? *)
     try Some (matching_params env loc params targs cargs)
     with Not_found -> None

(* Return a possible binding.  For example the goal is:
 *   goal compile (name) = "%name.o": "%name.c" {}
 * which means that params = ["name"] and targs = ["%name.o"].
 *
 * If we are called with cargs = ["file1.o"], we would
 * return ["file1"].
 *
 * On non-matching this raises Not_found.
 *)
and matching_params env loc params targs cargs =
  (* This is going to record the resulting binding. *)
  let res = ref Ast.Env.empty in
  List.iter2 (matching_param env loc params res) targs cargs;

  (* Rearrange the result into goal parameter order.  Also this
   * checks that every parameter got a binding.
   *)
  let res = !res in
  List.map (
    (* Allow the Not_found exception to escape if no binding for this param. *)
    fun param -> Ast.Env.find param res
  ) params

(* If targ = "%name.o" and carg = "file.o" then this would set
 * name => "file" in !res.  If they don't match, raises Not_found.
 *)
and matching_param env loc params res targ carg =
  match carg with
  | Ast.CString carg ->
     (* Substitute any non parameters in targ from the environment. *)
     let targ =
       List.map (
         function
         | Ast.SString _ as s -> s
         | Ast.SVar name ->
            if not (List.mem name params) then (
              try
                let expr = Ast.getvar env loc name in
                match Ast.to_constant env expr with
                | Ast.CString s -> Ast.SString s
              with Failure _ -> raise Not_found
            )
            else
              Ast.SVar name
       ) targ in

     (* Do the actual pattern matching.  Any remaining SVar elements
      * must refer to goal parameters.
      *)
     let carg = ref carg in
     let rec loop = function
       | [] ->
          (* End of targ, we must have matched all of carg. *)
          if !carg <> "" then raise Not_found
       | Ast.SString s :: rest ->
          (* Does this match the first part of !carg? *)
          let clen = String.length !carg in
          let slen = String.length s in
          if slen > clen || s <> String.sub !carg 0 slen then
            raise Not_found;
          (* Yes, so continue after the matching prefix. *)
          carg := String.sub !carg slen (clen-slen);
          loop rest
       | Ast.SVar name :: Ast.SString s :: rest ->
          (* This is a goal parameter.  Find s later in !carg. *)
          let i = string_find !carg s in
          if i = -1 then raise Not_found;
          (* Set the binding in !res. *)
          let r = Ast.EConstant (Ast.noloc,
                                 Ast.CString (String.sub !carg 0 i)) in
          res := Ast.Env.add name r !res;
          (* Continue after the match. *)
          let skip = i + String.length s in
          carg := String.sub !carg skip (String.length !carg - skip);
          loop rest
       | Ast.SVar name :: [] ->
          (* Matches the whole remainder of the string. *)
          let r = Ast.EConstant (Ast.noloc, Ast.CString !carg) in
          res := Ast.Env.add name r !res
       | Ast.SVar x :: Ast.SVar y :: _ ->
          (* TODO! We cannot match a target like "%x%y". *)
          assert false
     in
     loop targ
