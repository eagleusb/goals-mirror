(* Goalfile utilities
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

let failwithf fs = ksprintf failwith fs

let (//) = Filename.concat
let is_directory d = try Sys.is_directory d with Sys_error _ -> false

let unique = let i = ref 0 in fun () -> incr i; !i

(* From OCaml 4.08 sources.  We can remove this when we can
 * depend on min OCaml 4.08.
 *)
let filter_map f =
  let rec aux accu = function
    | [] -> List.rev accu
    | x :: l ->
        match f x with
        | None -> aux accu l
        | Some v -> aux (v :: accu) l
  in
  aux []


(* From libguestfs sources. *)
let rec string_find s sub =
  let len = String.length s in
  let sublen = String.length sub in
  let rec loop i =
    if i <= len-sublen then (
      let rec loop2 j =
        if j < sublen then (
          if s.[i+j] = sub.[j] then loop2 (j+1)
          else -1
        ) else
          i (* found *)
      in
      let r = loop2 0 in
      if r = -1 then loop (i+1) else r
    ) else
      -1 (* not found *)
  in
  loop 0

let rec split sep str =
  let len = String.length sep in
  let seplen = String.length str in
  let i = string_find str sep in
  if i = -1 then str, ""
  else (
    String.sub str 0 i, String.sub str (i + len) (seplen - i - len)
  )

and nsplit ?(max = 0) sep str =
  if max < 0 then
    invalid_arg "String.nsplit: max parameter should not be negative";

  (* If we reached the limit, OR if the pattern does not match the string
   * at all, return the rest of the string as a single element list.
   *)
  if max = 1 || string_find str sep = -1 then
    [str]
  else (
    let s1, s2 = split sep str in
    let max = if max = 0 then 0 else max - 1 in
    s1 :: nsplit ~max sep s2
  )

let isspace c =
  c = ' '
  (* || c = '\f' *) || c = '\n' || c = '\r' || c = '\t' (* || c = '\v' *)

let triml ?(test = isspace) str =
  let i = ref 0 in
  let n = ref (String.length str) in
  while !n > 0 && test str.[!i]; do
    decr n;
    incr i
  done;
  if !i = 0 then str
  else String.sub str !i !n

let trimr ?(test = isspace) str =
  let n = ref (String.length str) in
  while !n > 0 && test str.[!n-1]; do
    decr n
  done;
  if !n = String.length str then str
  else String.sub str 0 !n

let trim ?(test = isspace) str =
  trimr ~test (triml ~test str)

let absolute_path path =
  if not (Filename.is_relative path) then path else Sys.getcwd () // path
