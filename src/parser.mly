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
open Printf
%}

(* Tokens. *)
%token <Ast.substs> CODE
%token COLON
%token COMMA
%token <string> ID
%token EQUALS
%token EOF
%token GOAL
%token LEFT_ARRAY
%token LEFT_PAREN
%token LET
%token RIGHT_ARRAY
%token RIGHT_PAREN
%token <Ast.substs> STRING

%type <Ast.expr> expr
%type <Ast.stmt> stmt

(* Start non-terminal. *)
%start <Ast.file> file
%%

file:
    | stmts EOF  { $1 }
    ;

stmts:
    | list(stmt) { $1 }
    ;
stmt:
    | option(goal_stmt) patterns COLON barelist option(CODE)
    { let name, params =
        match $1 with
        | None ->
           let pos = $startpos in
           sprintf "_goal@%d" pos.pos_lnum, []
        | Some x -> x in
      Ast.Goal (name, params, $2, $4, $5)
    }
    | goal_stmt CODE
    {
      let name, params = $1 in
      Ast.Goal (name, params, [], [], Some $2)
    }
    | LET ID EQUALS expr { Ast.Let ($2, $4) }
    ;

goal_stmt:
    | GOAL ID option(param_decl) EQUALS
    { $2, match $3 with None -> [] | Some ps -> ps }
    ;
param_decl:
    | LEFT_PAREN separated_list(COMMA, ID) RIGHT_PAREN { $2 }
    ;

patterns:
    | separated_list(COMMA, pattern) { $1 }
    ;
pattern:
    | STRING     { Ast.PTactic ("file", [$1]) }
    | ID pattern_params { Ast.PTactic ($1, $2) }
    | ID         { Ast.PVarSubst $1 }
    ;
pattern_params:
    | LEFT_PAREN separated_list(COMMA, pattern_param) RIGHT_PAREN { $2 }
    ;
pattern_param:
    | STRING     { $1 }
    ;

expr:
    | ID params  { Ast.ECall ($1, $2) }
    | ID         { Ast.EVar $1 (* This might be replaced with ECall later. *) }
    | STRING     { Ast.EString $1 }
    | LEFT_ARRAY barelist RIGHT_ARRAY { Ast.EList $2 }
    ;
barelist:
    | separated_list(COMMA, expr) { $1 }
    ;
params:
    | LEFT_PAREN separated_list(COMMA, expr) RIGHT_PAREN { $2 }
    ;
