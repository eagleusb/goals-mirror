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

val failwithf : ('a, unit, string, 'b) format4 -> 'a
(** Like [failwith] but supports printf-like arguments. *)

val (//) : string -> string -> string
(** The {!Filename.concat} function. *)

val is_directory : string -> bool
(** Return true iff parameter is a directory. *)

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
(** [filter_map f l] applies [f] to every element of [l], filters
    out the [None] elements and returns the list of the arguments of
    the [Some] elements. *)

val string_find : string -> string -> int
(** [string_find str sub] finds the index of [sub] in [str].  If
    not found, returns -1. *)

val split : string -> string -> string * string
(** [split sep str] splits [str] at the first occurrence of the
    separator [sep], returning the part before and the part after.
    If separator is not found, return the whole string and an
    empty string. *)

val nsplit : ?max:int -> string -> string -> string list
(** [nsplit ?max sep str] splits [str] into multiple strings at each
    separator [sep].

    As with the Perl split function, you can give an optional
    [?max] parameter to limit the number of strings returned.  The
    final element of the list will contain the remainder of the
    input string. *)

val isspace : char -> bool
val triml : ?test:(char -> bool) -> string -> string
val trimr : ?test:(char -> bool) -> string -> string
val trim : ?test:(char -> bool) -> string -> string
(** Trim strings at left, right or both. *)

val absolute_path : string -> string
(** Convert any path into an absolute path. *)
