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

(* There are several circular dependencies between the lexer
 * (caused by includes) and eval.  These references break
 * the circular dependencies.  They are initialized when
 * the program starts, hence are never really None.
 *)
let lexer_read = ref None
let eval_substitute = ref None

let find_on_include_path filename =
  if not (Filename.is_implicit filename) then filename
  else (
    let rec loop = function
      | [] -> filename
      | inc :: incs ->
         let path = inc // filename in
         if Sys.file_exists path then path else loop incs
    in
    loop (Cmdline.includes ())
  )

let do_include env loc filename optflag file =
  let eval_substitute =
    match !eval_substitute with None -> assert false | Some f -> f in
  let filename = eval_substitute env loc filename in
  let filename = find_on_include_path filename in
  if optflag && not (Sys.file_exists filename) then env
  else (
    let fp = open_in filename in
    let lexbuf = Lexing.from_channel fp in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    let lexer_read =
      match !lexer_read with None -> assert false | Some r -> r in
    let env' = file lexer_read lexbuf in
    close_in fp;
    Ast.Env.merge env env'
  )
%}

(* Tokens. *)
%token <Ast.code> CODE
%token COLON
%token COMMA
%token EQUALS
%token EOF
%token EXPRESSION
%token FUNCTION
%token GOAL
%token <string> ID
%token INCLUDE
%token LEFT_ARRAY
%token LEFT_PAREN
%token LET
%token OPTINCLUDE
%token PURE
%token RETURNING
%token RIGHT_ARRAY
%token RIGHT_PAREN
%token SEMICOLON
%token <Ast.substs> STRING
%token STRING_KEYWORD
%token STRINGS
%token <string> PRED
%token PREDICATE

(* Start nonterminals. *)
%start <Ast.expr Ast.Env.t> file
%start <Ast.expr> expr_only
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
    | GOAL ID
    {
      $2, Ast.EGoalDefn ($loc, ([], [], [], None))
    }
    | option(PURE) FUNCTION ID params_decl return_decl EQUALS CODE
    {
      $3, Ast.EFuncDefn ($loc, ($4, $5, $1 <> None, $7))
    }
    | PREDICATE PRED params_decl EQUALS CODE
    {
      $2, Ast.EPredDefn ($loc, ($3, $5))
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
return_decl:
    |            { RetExpr }
    | RETURNING EXPRESSION { RetExpr }
    | RETURNING STRINGS { RetStrings }
    | RETURNING STRING_KEYWORD { RetString }

patterns:
    | separated_list(COMMA, pattern) { $1 }
    ;
pattern:
    | STRING     { Ast.PPred ($loc, "is-file", [$1]) }
    | PRED pattern_params { Ast.PPred ($loc, $1, $2) }
    ;
pattern_params:
    | LEFT_PAREN separated_list(COMMA, pattern_param) RIGHT_PAREN { $2 }
    ;
pattern_param:
    | STRING     { $1 }
    ;

expr:
    | ID params  { Ast.ECall ($loc, $1, $2) }
    | ID         { Ast.EVar ($loc, $1) }
    | PRED params { Ast.EPredCtor ($loc, $1, $2) }
    | STRING     { Ast.ESubsts ($loc, $1) }
    | LEFT_ARRAY barelist RIGHT_ARRAY { Ast.EList ($loc, $2) }
    ;
barelist:
    | right_flexible_list(COMMA, expr) { $1 }
    ;
params:
    | LEFT_PAREN separated_list(COMMA, expr) RIGHT_PAREN { $2 }
    ;

(* This is used by Parse.parse_expr where we have to parse
 * a standalone string (eg. from the command line).
 *)
expr_only:
    | expr EOF   { $1 }
    ;

(* http://gallium.inria.fr/blog/lr-lists/ *)
right_flexible_list(delim, X):
    | (* nothing *) { [] }
    | x = X { [x] }
    | x = X delim xs = right_flexible_list(delim, X) { x :: xs }
