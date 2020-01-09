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

open Lexing
open Printf

open Utils
open Lexer

let string_position () lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

let parse_file env lexbuf =
  try
    let env' = Parser.file Lexer.read lexbuf in
    Ast.Env.merge env env'
  with
  | SyntaxError msg ->
     failwithf "%a: %s" string_position lexbuf msg
  | Parser.Error ->
     failwithf "%a: parse error" string_position lexbuf

let parse_expr lexbuf =
  try Parser.expr Lexer.read lexbuf
  with
  | SyntaxError msg ->
     failwithf "%a: %s" string_position lexbuf msg
  | Parser.Error ->
     failwithf "%a: parse error" string_position lexbuf

(* This is used to parse the Goalfile. *)
let parse_goalfile env filename =
  Cmdline.debug "parse file: %s" filename;
  let fp = open_in filename in
  let lexbuf = Lexing.from_channel ~with_positions:true fp in
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_fname = filename };
  let env' = parse_file env lexbuf in
  close_in fp;
  env'

(* This is used to parse expressions on the command line and
 * the output from functions.
 *)
let parse_expr source str =
  Cmdline.debug "parse expression: %S" str;
  let lexbuf = Lexing.from_string ~with_positions:true str in
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_fname = source };
  parse_expr lexbuf
