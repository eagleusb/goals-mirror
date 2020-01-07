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

open Printf

open Utils

let pure_cache = Hashtbl.create 13

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

  | ECall (loc, name, args) ->
     let expr = Ast.getvar env loc name in
     (match expr with
      | EGoalDefn _ ->
         (* Goals don't return anything so they cannot be used in
          * constant expressions.  Use a function instead.
          *)
         failwithf "%a: cannot use goal call ‘%s’ in shell expansion"
           Ast.string_loc loc name

      | EFuncDefn (loc, func) ->
         to_constant env (call_function env loc name args func)

      | _ ->
         failwithf "%a: cannot use ‘%s’ in constant expression"
           Ast.string_loc loc name
     )

  | ETacticCtor (loc, name, _) ->
     failwithf "%a: cannot use tactic ‘%s’ in constant expression"
       Ast.string_loc loc name

  | EGoalDefn (loc, _) ->
     failwithf "%a: cannot use goal in constant expression"
       Ast.string_loc loc

  | EFuncDefn (loc, _) ->
     failwithf "%a: cannot use function in constant expression"
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

and to_shell_script env loc substs =
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

  | ECall (loc, name, args) ->
     let expr = Ast.getvar env loc name in
     (match expr with
      | EGoalDefn _ ->
         (* Goals don't return anything so they cannot be used in
          * shell script expansions.  Use a function instead.
          *)
         failwithf "%a: cannot use goal call ‘%s’ in shell expansion"
           Ast.string_loc loc name

      | EFuncDefn (loc, func) ->
         expr_to_shell_string env (call_function env loc name args func)

      | _ ->
         failwithf "%a: cannot call ‘%s’ which is not a function"
           Ast.string_loc loc name
     )

  (* Tactics expand to the first parameter. *)
  | ETacticCtor (loc, _, []) -> Filename.quote ""
  | ETacticCtor (loc, _, (arg :: _)) -> expr_to_shell_string env arg

  | EGoalDefn (loc, _) ->
     failwithf "%a: cannot use goal in shell expansion"
       Ast.string_loc loc

  | EFuncDefn (loc, _) ->
     failwithf "%a: cannot use function in shell expansion"
       Ast.string_loc loc

  | ETacticDefn (loc, _) ->
     failwithf "%a: cannot use tactic in shell expansion"
       Ast.string_loc loc

and run_code env loc code =
  let code = prepare_code env loc code in
  Sys.command code

and run_code_to_string env loc code =
  let code = prepare_code env loc code in
  let chan = Unix.open_process_in code in
  let b = Buffer.create 1024 in
  (try
     while true do
       Buffer.add_string b (input_line chan);
       Buffer.add_char b '\n'
     done
   with End_of_file -> ());
  let st = Unix.close_process_in chan in
  let i =
    match st with
    | Unix.WEXITED i -> i
    | Unix.WSIGNALED i ->
       failwithf "%a: killed by signal %d" Ast.string_loc loc i
    | Unix.WSTOPPED i ->
       failwithf "%a: stopped by signal %d" Ast.string_loc loc i in
  i, Buffer.contents b

and run_code_to_string_list env loc code =
  let code = prepare_code env loc code in
  let chan = Unix.open_process_in code in
  let lines = ref [] in
  (try while true do lines := input_line chan :: !lines done
   with End_of_file -> ());
  let st = Unix.close_process_in chan in
  let i =
    match st with
    | Unix.WEXITED i -> i
    | Unix.WSIGNALED i ->
       failwithf "%a: killed by signal %d" Ast.string_loc loc i
    | Unix.WSTOPPED i ->
       failwithf "%a: stopped by signal %d" Ast.string_loc loc i in
  let lines = List.rev !lines in
  i, lines

and prepare_code env loc (code, quiet) =
  let quiet = if Cmdline.debug_flag then false else quiet in
  let code = to_shell_script env loc code in
  "source " ^ Filename.quote Cmdline.prelude_sh_file ^ "\n" ^
  "set -e\n" ^
  (if not quiet then "set -x\n" else "") ^
  "\n" ^
  code

and evaluate_goal_arg env = function
  | Ast.EVar (loc, name) ->
     let expr = Ast.getvar env loc name in
     evaluate_goal_arg env expr

  | ESubsts (loc, str) ->
     let str = substitute env loc str in
     Ast.EConstant (loc, Ast.CString str)

  | EList (loc, exprs) ->
     Ast.EList (loc, List.map (evaluate_goal_arg env) exprs)

  | ETacticCtor (loc, name, exprs) ->
     Ast.ETacticCtor (loc, name, List.map (evaluate_goal_arg env) exprs)

  | ECall (loc, name, args) ->
     let expr = Ast.getvar env loc name in
     (match expr with
      | EGoalDefn _ ->
         (* Goals don't return anything so they cannot be used in
          * goal args.  Use a function instead.
          *)
         failwithf "%a: cannot use goal call ‘%s’ in goal argument"
           Ast.string_loc loc name

      | EFuncDefn (loc, func) ->
         call_function env loc name args func

      | _ ->
         failwithf "%a: cannot call ‘%s’ which is not a function"
           Ast.string_loc loc name
     )

  | EConstant _
  | EGoalDefn _
  | EFuncDefn _
  | ETacticDefn _ as e -> e

(* Functions are only called from goal args or when substituting
 * into a shell script or constant expression (this may change if we
 * implement ‘:=’ assignment for variables).  This evaluates a
 * function by running the associated shell script and parsing
 * the output as an expression, string or list of strings.
 *)
and call_function env loc name args (params, returning, pure, code) =
  (* This is used to print the function in debug and error messages only. *)
  let debug_func =
    sprintf "%s (%s) returning %s" name
      (String.concat ", " (List.map (Ast.string_expr ()) args))
      (match returning with RetExpr -> "expression"
                          | RetString -> "string"
                          | RetStrings -> "strings") in
  Cmdline.debug "%a: running function %s" Ast.string_loc loc debug_func;

  (* Evaluate function args.  Must be done before updating the environment. *)
  let args = List.map (evaluate_goal_arg env) args in

  (* Create a new environment which maps the parameter names to
   * the args.
   *)
  let env =
    let params =
      try List.combine params args
      with Invalid_argument _ ->
        failwithf "%a: calling function %s with wrong number of arguments, expecting %d args but got %d args"
          Ast.string_loc loc debug_func
          (List.length params) (List.length args) in
    List.fold_left (fun env (k, v) -> Ast.Env.add k v env) env params in

  if pure then call_function_pure env loc name returning code
  else call_function_really env loc name returning code

and call_function_really env loc name returning code =
  match returning with
  | RetExpr ->
     let r, b = run_code_to_string env loc code in
     if r <> 0 then (
       eprintf "*** function ‘%s’ failed with exit code %d\n" name r;
       exit 1
     );
     Parse.parse_expr (sprintf "function:%s" name) b

  | RetString ->
     let r, b = run_code_to_string env loc code in
     if r <> 0 then (
       eprintf "*** function ‘%s’ failed with exit code %d\n" name r;
       exit 1
     );
     Ast.EConstant (loc, Ast.CString b)

  | RetStrings ->
     let r, lines = run_code_to_string_list env loc code in
     let strs = List.map (fun s -> Ast.EConstant (loc, Ast.CString s)) lines in
     EList (loc, strs)

(* For pure functions, check if the function can be matched to
 * a previously memoized result, but don't fail if this goes wrong.
 *)
and call_function_pure env loc name returning code =
  let code_key =
    try Some (to_shell_script env loc (fst code))
    with Failure _ -> None in
  match code_key with
  | None -> call_function_really env loc name returning code
  | Some code_key ->
     let r =
       try Some (Hashtbl.find pure_cache code_key)
       with Not_found -> None in
     match r with
     | Some expr -> expr
     | None ->
        let expr = call_function_really env loc name returning code in
        Hashtbl.add pure_cache code_key expr;
        expr
