(* Goalfile parser
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

%{
open Utils
open Printf

(* This is initialized with Lexer.read once the program
 * starts.  Doing this avoids a circular dependency caused
 * by include files.
 *)
let lexer_read = ref None

let find_on_include_path filename =
  if not (Filename.is_implicit filename) then filename
  else (
    let rec loop = function
      | [] -> filename
      | inc :: incs ->
         let path = inc // filename in
         if Sys.file_exists path then path else loop incs
    in
    loop Cmdline.includes
  )

let do_include env loc filename optflag file =
  let filename = Ast.substitute env loc filename in
  let filename = find_on_include_path filename in
  if optflag && not (Sys.file_exists filename) then env
  else (
    let fp = open_in filename in
    let lexbuf = Lexing.from_channel fp in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    let reader =
      match !lexer_read with None -> assert false | Some r -> r in
    let env' = file reader lexbuf in
    close_in fp;
    Ast.Env.merge env env'
  )
%}

(* Tokens. *)
%token <Ast.substs> CODE
%token COLON
%token COMMA
%token EQUALS
%token EOF
%token GOAL
%token <string> ID
%token INCLUDE
%token LEFT_ARRAY
%token LEFT_PAREN
%token LET
%token OPTINCLUDE
%token RIGHT_ARRAY
%token RIGHT_PAREN
%token SEMICOLON
%token <Ast.substs> STRING
%token <string> TACTIC
%token TACTIC_KEYWORD

(* Start nonterminals. *)
%start <Ast.expr Ast.Env.t> file
%start <Ast.expr> expr
%%

file:
    | stmts EOF  { $1 }
    ;

stmts:
    | (* none *) { Ast.Env.empty }
    | stmts INCLUDE STRING option(SEMICOLON)
    { do_include $1 $loc $3 false file }
    | stmts OPTINCLUDE STRING option(SEMICOLON)
    { do_include $1 $loc $3 true file }
    | stmts stmt option(SEMICOLON)
    { let name, expr = $2 in Ast.Env.add name expr $1 }
    ;

stmt:
    | option(goal_stmt) patterns COLON barelist option(CODE)
    { let name, params =
        match $1 with
        | None ->
           sprintf "_goal@%d" $startpos.pos_lnum, []
        | Some x -> x in
      name, Ast.EGoalDefn ($loc, (params, $2, $4, $5))
    }
    | goal_stmt CODE
    {
      let name, params = $1 in
      name, Ast.EGoalDefn ($loc, (params, [], [], Some $2))
    }
    | TACTIC_KEYWORD TACTIC params_decl EQUALS CODE
    {
      $2, Ast.ETacticDefn ($loc, ($3, $5))
    }
    | LET ID EQUALS expr { $2, $4 }
    ;

goal_stmt:
    | GOAL ID option(params_decl) EQUALS
    { $2, match $3 with None -> [] | Some ps -> ps }
    ;
params_decl:
    | LEFT_PAREN separated_list(COMMA, param_decl) RIGHT_PAREN { $2 }
    ;
param_decl:
    | ID         { $1 }

patterns:
    | separated_list(COMMA, pattern) { $1 }
    ;
pattern:
    | STRING     { Ast.PTactic ($loc, "*file", [$1]) }
    | ID pattern_params { Ast.PTactic ($loc, $1, $2) }
    ;
pattern_params:
    | LEFT_PAREN separated_list(COMMA, pattern_param) RIGHT_PAREN { $2 }
    ;
pattern_param:
    | STRING     { $1 }
    ;

expr:
    | ID params  { Ast.ECallGoal ($loc, $1, $2) }
    | ID         { Ast.EVar ($loc, $1) }
    | TACTIC params { Ast.ETacticCtor ($loc, $1, $2) }
    | STRING     { Ast.ESubsts ($loc, $1) }
    | LEFT_ARRAY barelist RIGHT_ARRAY { Ast.EList ($loc, $2) }
    ;
barelist:
    | separated_list(COMMA, expr) { $1 }
    ;
params:
    | LEFT_PAREN separated_list(COMMA, expr) RIGHT_PAREN { $2 }
    ;
