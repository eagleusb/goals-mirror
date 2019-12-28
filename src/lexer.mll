(* Goalfile lexer
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

{
open Lexing
open Parser

open Printf

exception SyntaxError of string

let new_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos; pos_lnum = pos.pos_lnum + 1 }
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let comment = '#' (_#'\n')*
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
    parse
    | white
    | comment { read lexbuf }
    | newline { new_line lexbuf; read lexbuf }
    | ","     { COMMA }
    | ":"     { COLON }
    | "="     { EQUALS }
    | "("     { LEFT_PAREN }
    | ")"     { RIGHT_PAREN }
    | "["     { LEFT_ARRAY }
    | "]"     { RIGHT_ARRAY }
    | '"'     { read_string (Ast.Substs.create ()) lexbuf }
    | "{"     { read_code (Ast.Substs.create ()) (ref 1) lexbuf }
    | "goal"  { GOAL }
    | "tactic" { TACTIC_KEYWORD }
    | "let"   { LET }
    | "include" { INCLUDE }
    | "*" id  { (* NB: The initial '*' is part of the name. *)
                TACTIC (Lexing.lexeme lexbuf) }
    | id      { ID (Lexing.lexeme lexbuf) }
    | _       { raise (SyntaxError ("unexpected character: " ^
                                    Lexing.lexeme lexbuf)) }
    | eof     { EOF }

(* Parse "STRING" literal with %-substitutions. *)
and read_string buf =
    parse
    | '\\' '"'
              { Ast.Substs.add_char buf '"'; read_string buf lexbuf }
    | '\\' 'a'
              { Ast.Substs.add_char buf '\007'; read_string buf lexbuf }
    | '\\' 'b'
              { Ast.Substs.add_char buf '\008'; read_string buf lexbuf }
    | '\\' 't'
              { Ast.Substs.add_char buf '\009'; read_string buf lexbuf }
    | '\\' 'n'
              { Ast.Substs.add_char buf '\010'; read_string buf lexbuf }
    | '\\' 'v'
              { Ast.Substs.add_char buf '\011'; read_string buf lexbuf }
    | '\\' 'f'
              { Ast.Substs.add_char buf '\012'; read_string buf lexbuf }
    | '\\' 'r'
              { Ast.Substs.add_char buf '\013'; read_string buf lexbuf }
    | '\\' '\\'
              { Ast.Substs.add_char buf '\\'; read_string buf lexbuf }
    | '"'     { STRING (Ast.Substs.get buf) }
    | newline { Ast.Substs.add_char buf '\n';
                new_line lexbuf; read_string buf lexbuf }
    | '%' '%' { Ast.Substs.add_char buf '%'; read_string buf lexbuf }
    | '%' id  { let id = Lexing.lexeme lexbuf in
                let len = String.length id in
                Ast.Substs.add_var buf (String.sub id 1 (len-1));
                read_string buf lexbuf }
    | '%' _   { raise (SyntaxError ("illegal character in %-substitution: " ^
                                    Lexing.lexeme lexbuf)) }
    | [^ '"' '\\' '\r' '\n' '%' ]+
              { Ast.Substs.add_string buf (Lexing.lexeme lexbuf);
                read_string buf lexbuf }
    | _       { raise (SyntaxError ("illegal character in string: " ^
                                      Lexing.lexeme lexbuf)) }
    | eof     { raise (SyntaxError ("unterminated string")) }

(* Parse { CODE } literal with %-substitutions.
 *
 * Note the range of %-substitutions possible is larger than
 * for strings.
 *)
and read_code buf level =
    parse
    | '{'     { incr level; read_code buf level lexbuf }
    | '}'     { decr level;
                if !level = 0 then CODE (Ast.Substs.get buf)
                else (
                  Ast.Substs.add_char buf '}';
                  read_code buf level lexbuf
                ) }
    | newline { Ast.Substs.add_char buf '\n';
                new_line lexbuf; read_code buf level lexbuf }
    | '%' '%' { Ast.Substs.add_char buf '%'; read_code buf level lexbuf }
    | '%' '@' { Ast.Substs.add_var buf "@"; read_code buf level lexbuf }
    | '%' '<' { Ast.Substs.add_var buf "<"; read_code buf level lexbuf }
    | '%' '^' { Ast.Substs.add_var buf "^"; read_code buf level lexbuf }
    | '%' id  { let id = Lexing.lexeme lexbuf in
                let len = String.length id in
                Ast.Substs.add_var buf (String.sub id 1 (len-1));
                read_code buf level lexbuf }
    | '%' _   { raise (SyntaxError ("illegal character in %-substitution: " ^
                                      Lexing.lexeme lexbuf)) }
    | [^ '{' '}' '\r' '\n' '%' ]+
              { Ast.Substs.add_string buf (Lexing.lexeme lexbuf);
                read_code buf level lexbuf }
    | _       { raise (SyntaxError ("illegal character in code section: " ^
                                      Lexing.lexeme lexbuf)) }
    | eof     { raise (SyntaxError ("unterminated code section")) }
