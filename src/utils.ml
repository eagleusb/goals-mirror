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
