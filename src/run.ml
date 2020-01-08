(* Goalfile run
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

(* Goals uses the goal (name + parameters) as the key to
 * ensure you cannot have two jobs running at the same time
 * which would interfere with each other by trying to build
 * the same target.
 *)
module Jobs = Jobs.Make (
  struct
    type t = string * Ast.expr list
    let compare = compare
    let to_string (name, args) =
      sprintf "%s (%s)" name
        (String.concat ", " (List.map (Ast.string_expr ()) args))
  end
)

(* Starts the target expressions running and waits for them to complete. *)
let rec run_targets_to_completion env exprs =
  let group = Jobs.new_group () in
  run_targets group env exprs;
  Jobs.wait group

(* This starts the targets, adding them to the jobs group, but does not
 * wait for them to complete.
 *)
and run_targets group env exprs =
  List.iter (run_target group env) exprs

(* This starts a single target, adding the (usually single but can
 * be multiple) jobs to the jobs group.  It does not wait for the
 * jobs to complete.
 *)
and run_target group env = function
  | Ast.EGoalDefn _ | Ast.EFuncDefn _ | Ast.ETacticDefn _ -> assert false

  (* Call a goal or function. *)
  | Ast.ECall (loc, name, args) ->
     let expr = Ast.getvar env loc name in
     (match expr with
      | Ast.EGoalDefn (_, goal) ->
         let key = name, args in
         Jobs.start group key (
           fun () -> run_goal env loc name args goal []
         )
      | Ast.EFuncDefn (_, func) ->
         let expr = Eval.call_function env loc name args func in
         run_target group env expr
      | _ ->
         failwithf "%a: tried to call ‘%s’ which is not a goal or a function"
           Ast.string_loc loc name
     )

  (* Call a tactic. *)
  | Ast.ETacticCtor (loc, name, args) ->
     (* All parameters of tactics must be simple constant expressions
      * (strings, in future booleans, numbers, etc).
      *)
     let args = List.map (Eval.to_constant env) args in
     run_tactic group env loc name args

  (* If this is a goal then it's the same as calling goal().  If not
   * then look up the variable and substitute it.
   *)
  | Ast.EVar (loc, name) ->
     let expr = Ast.getvar env loc name in
     (match expr with
      | Ast.EGoalDefn (loc, ([], _, _, _)) ->
         run_target group env (Ast.ECall (loc, name, []))
      | EGoalDefn _ ->
         failwithf "%a: cannot call %s() since this goal has parameters"
           Ast.string_loc loc name
      | _ ->
         run_target group env expr
     )

  (* Lists are inlined when found as a target. *)
  | Ast.EList (loc, exprs) ->
     run_targets group env exprs

  (* A string (with or without substitutions) implies *file(filename). *)
  | Ast.ESubsts (loc, str) ->
     let str = Eval.substitute env loc str in
     run_tactic group env loc "*file" [Ast.CString str]

  | Ast.EConstant (loc, c) ->
     run_tactic group env loc "*file" [c]

(* Run a goal by name. *)
and run_goal env loc name args (params, patterns, deps, code) extra_deps =
  (* This is used to print the goal in debug and error messages only. *)
  let debug_goal =
    sprintf "%s (%s)" name
      (String.concat ", " (List.map (Ast.string_expr ()) args)) in
  Cmdline.debug "%a: running goal %s" Ast.string_loc loc debug_goal;

  (* This is the point where we evaluate the goal arguments.  We must
   * do this before creating the new environment, because variables
   * appearing in goal arguments don't refer to goal parameters.
   *)
  let args = List.map (Eval.evaluate_goal_arg env) args in

  (* Create a new environment which maps the parameter names to
   * the args.
   *)
  let env =
    let params =
      try List.combine params args
      with Invalid_argument _ ->
        failwithf "%a: calling goal %s with wrong number of arguments, expecting %d args but got %d args"
          Ast.string_loc loc debug_goal
          (List.length params) (List.length args) in
    List.fold_left (fun env (k, v) -> Ast.Env.add k v env) env params in

  (* Check all dependencies have been updated.  We must wait
   * for these to complete before we can continue.
   *)
  run_targets_to_completion env (deps @ extra_deps);

  (* Check if any target (ie. pattern) needs to be rebuilt.
   * As with make, a goal with no targets is always run.
   *)
  let rebuild =
    patterns = [] ||
    List.exists (needs_rebuild env loc deps extra_deps) patterns in

  if rebuild then (
    (* Run the code (if any). *)
    (match code with
     | None -> () (* No { CODE } section. *)

     | Some code ->
        (* Add some standard variables to the environment. *)
        let expr_of_substs s = Ast.ESubsts (Ast.noloc, s) in
        let expr_of_pattern = function
          | Ast.PTactic (loc, tactic, targs) ->
             Ast.ETacticCtor (loc, tactic, List.map expr_of_substs targs)
        in
        let pexprs = List.map expr_of_pattern patterns in
        let env = Ast.Env.add "@" (Ast.EList (Ast.noloc, pexprs)) env in
        let env = Ast.Env.add "<" (Ast.EList (Ast.noloc, deps)) env in
        let env =
          (* NB: extra_deps are not added to %^ *)
          match deps with
          | [] -> env
          | d :: _ -> Ast.Env.add "^" d env in
        let r = Eval.run_code env loc code in
        if r <> 0 then (
          eprintf "*** goal ‘%s’ failed with exit code %d\n" name r;
          exit 1
        );

        (* Check all targets were updated after the code was
         * run (else it's an error).
         *)
        let pattern_still_needs_rebuild =
          try
            Some (List.find (needs_rebuild env loc deps extra_deps) patterns)
          with
            Not_found -> None in
        match pattern_still_needs_rebuild with
        | None -> ()
        | Some pattern ->
           failwithf "%a: goal %s ran successfully but it did not rebuild %a"
             Ast.string_loc loc debug_goal Ast.string_pattern pattern
    )
  )

(* Return whether the target (pattern) needs to be rebuilt. *)
and needs_rebuild env loc deps extra_deps pattern =
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
     let targs = List.map (Eval.substitute env loc) targs in
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
       (* NB: extra_deps are not added to %^ *)
       match deps with
       | [] -> env
       | d :: _ -> Ast.Env.add "^" d env in
     let r = Eval.run_code env loc code in
     if r = 99 (* means "needs rebuild" *) then true
     else if r = 0 (* means "doesn't need rebuild" *) then false
     else (
       eprintf "*** tactic ‘%s’ failed with exit code %d\n" tactic r;
       exit 1
     )

(* Find the goal which matches the given tactic and start it.
 * cargs is a list of parameters (all constants).
 *)
and run_tactic group env loc tactic cargs =
  (* This is used to print the tactic in debug and error messages only. *)
  let debug_tactic =
    Ast.string_expr ()
      (Ast.ETacticCtor (loc, tactic,
                        List.map (fun c -> Ast.EConstant (loc, c)) cargs)) in
  Cmdline.debug "%a: running tactic %s" Ast.string_loc loc debug_tactic;

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
     if needs_rebuild env loc [] [] p then
       failwithf "%a: don't know how to build %s"
         Ast.string_loc loc debug_tactic

  | [_, name, goal, args] ->
     (* Single goal matches, run it. *)
     let key = name, args in
     Jobs.start group key (
       fun () -> run_goal env loc name args goal []
     )

  | goals ->
     (* Two or more goals match.  Only one must have a CODE section,
      * and we combine the dependencies into a "supergoal".
      *)
     let with_code, without_code =
       List.partition (
         fun (_, _, (_, _, _, code), _) -> code <> None
       ) goals in

     let (_, name, goal, args), extra_deps =
       match with_code with
       | [g] ->
          let extra_deps =
            List.flatten (
              List.map (fun (_, _, (_, _, deps, _), _) -> deps) without_code
            ) in
          (g, extra_deps)

       | [] ->
          (* This is OK, it means we'll rebuild all dependencies
           * but there is no code to run.  Pick the first goal
           * without code and the dependencies from the other goals.
           *)
          let g = List.hd without_code in
          let extra_deps =
            List.flatten (
              List.map (fun (_, _, (_, _, deps, _), _) -> deps)
                (List.tl without_code)
            ) in
          (g, extra_deps)

       | _ :: _ ->
          failwithf "%a: multiple goals found which match tactic %s, but more than one of these goals have {code} sections which is not allowed"
            Ast.string_loc loc debug_tactic in

     let key = name, args in
     Jobs.start group key (fun () ->
       run_goal env loc name args goal extra_deps
     )

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
                match Eval.to_constant env expr with
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
