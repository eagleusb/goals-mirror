(* Goalfile dependency DAG
 * Copyright (C) 2020 Richard W.M. Jones
 * Copyright (C) 2020 Red Hat Inc.
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

type node =
  | Goal of Ast.env * Ast.loc * string * Ast.expr list *
              Ast.goal * Ast.expr list * string
  | Exists of Ast.env * Ast.loc * Ast.pattern * string

let string_of_node = function
  | Goal (_, _, _, _, _, _, debug_goal) -> debug_goal
  | Exists (_, _, _, debug_tactic) -> debug_tactic

let compare_nodes n1 n2 =
  match n1, n2 with
  | Goal _, Exists _ -> -1
  | Exists _, Goal _ -> 1
  | Exists (_, _, p1, _), Exists (_, _, p2, _) -> compare p1 p2
  | Goal (_, _, n1, a1, _, _, _), Goal (_, _, n2, a2, _, _, _) ->
     compare (n1, a1) (n2, a2)

module G = Map.Make (
  struct
    type t = node
    let compare = compare_nodes
  end
)

type dag = {
  (* List of nodes. *)
  nodes : node list;

  (* Edges are stored as an adjacency list, which is a map from
   * a parent node to a list of child nodes.  Note that as the
   * graph does not need to be connected, there may be nodes
   * in the list above which don't appear in this map.
   *)
  edges : node list G.t;
}

type t = dag * node list
(* The final type is a DAG and a topologically sorted list of nodes. *)

(* Creates a new DAG. *)
let rec new_dag () = { nodes = []; edges = G.empty }

(* This will create a new node, unless the node already exists.
 * If the optional parent parameter is given then it also creates
 * an edge from parent to the new (or existing) node.
 *)
and add_node { nodes; edges } ?parent data =
  let node, nodes =
    try List.find (fun n -> compare_nodes n data = 0) nodes, nodes
    with Not_found -> data, data :: nodes in
  let edges =
    match parent with
    | None -> edges
    | Some parent ->
       let children = try G.find parent edges with Not_found -> [] in
       if List.mem node children then edges
       else G.add parent (node :: children) edges in
  node, { nodes; edges }

(* This is Khan's algorithm. *)
and topological_sort dag =
  let incoming_map = incoming_map dag in

  (* Set of all nodes with no incoming edge. *)
  let q = List.filter (fun node -> not (G.mem node incoming_map)) dag.nodes in

  let rec loop dag acc im = function
    | [] -> dag, acc
    | node :: q ->
       let acc = node :: acc in
       let children = try G.find node dag.edges with Not_found -> [] in
       let dag, q, im =
         List.fold_left (
           fun (dag, q, im) child ->
             (* There's an arrow from node to child. *)
             let dag =
               { nodes =
                   List.filter (fun n -> compare_nodes n node <> 0) dag.nodes;
                 edges = remove_edge dag.edges node child } in
             let im = remove_edge im child node in
             let q = if not (G.mem child im) then child :: q else q in
             (dag, q, im)
         ) (dag, q, im) children in
       loop dag acc im q
  in
  let dag, acc = loop dag [] incoming_map q in

  if not (G.is_empty dag.edges) then
    (* XXX More debugging to help out users!  I believe the remaining
     * edges should demonstrate the cycle.
     *)
    failwithf "dependency graph contains cycles";

  (* This builds the topological list in reverse order, but that's
   * fine because that is the running order.
   *)
  acc

(* The dag structure has an adjacency list, which is a list of outgoing
 * edges from each node.  But for a topological sort what we actually
 * need is another list of incoming edges, so construct that first.
 *
 * Note this never returns a mapping node -> [].
 *)
and incoming_map { edges } =
  let im = ref G.empty in
  G.iter (
    fun parent children ->
      List.iter (
        fun c ->
          (* There is an arrow from parent -> c. *)
          let xs = try G.find c !im with Not_found -> [] in
          im := G.add c (parent :: xs) !im
      ) children
  ) edges;
  !im

(* Remove edge from parent to child returning a new edges map.
 * Preserves the invariant that there is never a mapping node -> [].
 *)
and remove_edge edges parent child =
  try
    let children = G.find parent edges in
    let children =
      List.filter (fun n -> compare_nodes n child <> 0) children in
    if children = [] then
      G.remove parent edges
    else
      G.add parent children edges
  with
    Not_found -> edges

and debug_dag { nodes; edges } =
  eprintf "nodes:\n";
  List.iter (fun node -> eprintf "  %s\n" (string_of_node node)) nodes;
  eprintf "edges:\n";
  G.iter (
    fun parent children ->
      eprintf "  %s ->" (string_of_node parent);
      List.iter (fun c -> eprintf " %s" (string_of_node c)) children;
      eprintf "\n"
  ) edges

let rec create env roots =
  let dag = new_dag () in
  let dag = add_targets dag env roots in
  if Cmdline.debug_flag () then debug_dag dag;
  (* Make actually breaks cycles, but I'm not convinced that this
   * is a good idea, so this function will fail if any cycle is
   * found.  We may wish to revisit this decision in future.
   *)
  let sorted = topological_sort dag in
  if Cmdline.debug_flag () then
    eprintf "dependency order:\n  %s\n"
      (String.concat " <- " (List.map string_of_node sorted));
  dag, sorted

and add_targets dag ?parent env roots =
  List.fold_left (fun dag root -> add_target dag ?parent env root) dag roots

and add_target dag ?parent env = function
  | Ast.EGoalDefn _ | Ast.EFuncDefn _ | Ast.ETacticDefn _ -> assert false

  (* Call a goal or function. *)
  | Ast.ECall (loc, name, args) ->
     let expr = Ast.getvar env loc name in
     (match expr with
      | Ast.EGoalDefn (_, goal) ->
         add_goal dag ?parent env loc name args goal []
      | Ast.EFuncDefn (_, func) ->
         let expr = Eval.call_function env loc name args func in
         add_target dag ?parent env expr
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
     add_tactic dag ?parent env loc name args

  (* If this is a goal then it's the same as calling goal().  If not
   * then look up the variable and substitute it.
   *)
  | Ast.EVar (loc, name) ->
     let expr = Ast.getvar env loc name in
     (match expr with
      | Ast.EGoalDefn (loc, ([], _, _, _)) ->
         add_target dag ?parent env (Ast.ECall (loc, name, []))
      | EGoalDefn _ ->
         failwithf "%a: cannot call %s() since this goal has parameters"
           Ast.string_loc loc name
      | _ ->
         add_target dag ?parent env expr
     )

  (* Lists are inlined when found as a target. *)
  | Ast.EList (loc, exprs) ->
     add_targets dag ?parent env exprs

  (* A string (with or without substitutions) implies *file(filename). *)
  | Ast.ESubsts (loc, str) ->
     let str = Eval.substitute env loc str in
     add_tactic dag ?parent env loc "*file" [Ast.CString str]

  | Ast.EConstant (loc, c) ->
     add_tactic dag ?parent env loc "*file" [c]

(* Add a goal by name. *)
and add_goal dag ?parent env loc name args
             ((params, patterns, deps, code) as goal)
             extra_deps =
  (* This is used to print the goal in debug and error messages only. *)
  let debug_goal =
    sprintf "%s (%s)" name
      (String.concat ", " (List.map (Ast.string_expr ()) args)) in
  Cmdline.debug "%a: adding goal %s" Ast.string_loc loc debug_goal;

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

  (* Create the node. *)
  let node, dag =
    add_node dag ?parent (Goal (env, loc, name, args, goal,
                                extra_deps, debug_goal)) in

  (* Add all dependencies. *)
  add_targets dag ~parent:node env (deps @ extra_deps)

(* Find the goal which matches the given tactic and add it.
 * cargs is a list of parameters (all constants).
 *)
and add_tactic dag ?parent env loc tactic cargs =
  (* This is used to print the tactic in debug and error messages only. *)
  let debug_tactic =
    Ast.string_expr ()
      (Ast.ETacticCtor (loc, tactic,
                        List.map (fun c -> Ast.EConstant (loc, c)) cargs)) in
  Cmdline.debug "%a: adding tactic %s" Ast.string_loc loc debug_tactic;

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
      * the tactic doesn't need to be rebuilt.  So create a
      * special Exists node which will be used to run the tactic
      * to see if the target needs to be rebuilt, and only fail
      * if it does need a rebuild.
      *)
     let targs = List.map (function Ast.CString s -> [Ast.SString s]) cargs in
     let p = Ast.PTactic (loc, tactic, targs) in
     let _, dag = add_node dag ?parent (Exists (env, loc, p, debug_tactic)) in
     dag

  | [_, name, goal, args] ->
     (* Single goal matches. *)
     add_goal dag ?parent env loc name args goal []

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

     add_goal dag ?parent env loc name args goal extra_deps

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

type goal_runner =
  Ast.env -> Ast.loc -> string -> Ast.expr list -> Ast.goal ->
  Ast.expr list -> string -> unit

type exists_runner = Ast.env -> Ast.loc -> Ast.pattern -> string -> unit

type state = {
  dag : dag;
  goal_runner : goal_runner;
  exists_runner : exists_runner;

  (* Topologically sorted in build order.  When nodes start running
   * we take them off this list.
   *)
  mutable sorted_nodes : node list;

  (* List of nodes which completed successfully.  Actually for fast
   * access we store a map node -> true.
   *)
  mutable complete : bool G.t;

  (* List of nodes which failed. *)
  mutable failed : bool G.t;
}

let new_state (dag, sorted_nodes) goal_runner exists_runner =
  { dag; goal_runner; exists_runner; sorted_nodes;
    complete = G.empty; failed = G.empty }

(* Called by [Jobs] when a node completes successfully.  We mark
 * it as done.
 *)
let retire_job state node =
  state.complete <- G.add node true state.complete

(* Called by [Jobs] when a node fails.  We mark the node as
 * failed and ensure that anything that depends on it will
 * also be marked as failed (and never returned by next_job).
 *)
let fail_job state node =
  state.failed <- G.add node true state.failed

let rec next_job state =
  (* Find the earliest node in the list which has all its
   * dependencies done.
   *)
  let rec loop acc = function
    | [] ->
       if state.sorted_nodes = [] then Jobs.Complete else Jobs.Not_ready
    | node :: nodes when node_is_ready_to_run state node ->
       (* Drop the node from the list of jobs and run it. *)
       state.sorted_nodes <- List.rev acc @ nodes;
       (match node with
        | Goal (env, loc, name, args, goal, extra_deps, debug_goal) ->
           Jobs.Job (node, fun () ->
                           state.goal_runner env loc name args goal
                             extra_deps debug_goal)
        | Exists (env, loc, p, debug_tactic) ->
           Jobs.Job (node, fun () ->
                           state.exists_runner env loc p debug_tactic)
       )
    | node :: nodes when node_failed state node ->
       (* Mark it as failed also, and drop it from the list of jobs. *)
       fail_job state node;
       state.sorted_nodes <- List.rev acc @ nodes;
       loop acc nodes
    | node :: nodes ->
       (* All dependencies of this node are neither complete nor failed,
        * continue down the list.
        *)
       loop (node :: acc) nodes
  in
  loop [] state.sorted_nodes

(* All dependencies of this node are complete. *)
and node_is_ready_to_run { dag; complete } node =
  let parents = try G.find node dag.edges with Not_found -> [] in
  List.for_all (fun p -> G.mem p complete) parents

(* This node or any dependency of this node failed. *)
and node_failed { dag; failed } node =
  G.mem node failed || (
    let parents = try G.find node dag.edges with Not_found -> [] in
    List.exists (fun p -> G.mem p failed) parents
  )

let string_of_job  = string_of_node
