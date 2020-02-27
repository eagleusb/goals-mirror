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

let rec goal_runner env loc name args
                    (params, patterns, deps, code) extra_deps debug_goal =
  Cmdline.debug "%a: running goal %s" Ast.string_loc loc debug_goal;

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
          | Ast.PPred (loc, pred, targs) ->
             Ast.EPredCtor (loc, pred, List.map expr_of_substs targs)
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
        if r <> 0 then
          failwithf "%a: goal ‘%s’ failed with exit code %d"
            Ast.string_loc loc debug_goal r;

        (* Check all targets were updated after the code was
         * run (else it's an error).
         *)
        let pattern_still_needs_rebuild =
          try
            let pattern =
              List.find
                (needs_rebuild ~final_check:true env loc deps extra_deps)
                patterns in
            Some pattern
          with
            Not_found -> None in
        match pattern_still_needs_rebuild with
        | None -> ()
        | Some pattern ->
           failwithf "%a: goal ‘%s’ ran successfully but it did not rebuild %a"
             Ast.string_loc loc debug_goal Ast.string_pattern pattern
    )
  )

(* Return whether the target (pattern) needs to be rebuilt. *)
and needs_rebuild ?(final_check = false) env loc deps extra_deps pattern =
  Cmdline.debug "%a: testing if %a needs rebuild"
    Ast.string_loc loc Ast.string_pattern pattern;

  match pattern with
  | Ast.PPred (loc, pred, targs) ->
     (* Look up the predicate. *)
     let params, code = Ast.getpred env loc pred in

     (* Resolve the targs down to constants.  Since needs_rebuild
      * should be called with env containing the goal params, this
      * should substitute any parameters in the predicate arguments.
      *)
     let targs = List.map (Eval.substitute env loc) targs in
     let targs =
       List.map (fun targ ->
           Ast.EConstant (Ast.noloc, Ast.CString targ)) targs in

     (* Create a new environment binding parameter names
      * to predicate args.
      *)
     let env =
       let params =
         try List.combine params targs
         with Invalid_argument _ ->
           failwithf "%a: calling predicate ‘%s’ with wrong number of arguments"
             Ast.string_loc loc pred in
       List.fold_left (fun env (k, v) -> Ast.Env.add k v env) env params in

     (* Add some standard variables to the environment. *)
     let env = Ast.Env.add "<" (Ast.EList (Ast.noloc, deps)) env in
     let env =
       (*let b = Ast.EConstant (Ast.noloc, Ast.CBool final_check) in*)
       let b = Ast.EConstant (Ast.noloc,
                              Ast.CString (if final_check then "1" else "")) in
       Ast.Env.add "goals_final_check" b env in
     let env =
       (* NB: extra_deps are not added to %^ *)
       match deps with
       | [] -> env
       | d :: _ -> Ast.Env.add "^" d env in
     let r = Eval.run_code env loc code in
     if r = 99 (* means "needs rebuild" *) then true
     else if r = 0 (* means "doesn't need rebuild" *) then false
     else (
       let targs = List.map (Ast.string_expr ()) targs in
       let targs = String.concat ", " targs in
       failwithf "%a: predicate ‘%s (%s)’ failed with exit code %d"
         Ast.string_loc loc pred targs r
     )

and exists_runner env loc p debug_pred =
  Cmdline.debug "%a: running implicit existence rule for predicate %s"
    Ast.string_loc loc debug_pred;

  if needs_rebuild env loc [] [] p then
    failwithf "%a: don't know how to build ‘%s’"
      Ast.string_loc loc debug_pred
