(* Goals parallel jobs.
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

(** This module manages parallel jobs.

    Jobs are grouped.  You call [new_group] to create a new
    group of jobs, initially empty.  Then add jobs to it.  Then
    wait for all the jobs in the group to complete.

    To submit a job to a group use [start group key f].  [group]
    is an existing group of jobs to which this is added.  [key] is
    a key which ensures that two identical jobs cannot be running
    at the same time (across all groups).  If two or more jobs
    with the same key are submitted then only one will run and
    the others will wait until the first finishes, and then another
    will be picked to run and so on.  Jobs with different keys run
    freely in parallel, assuming there are enough threads available
    to run them.

    Goals uses the goal (name + parameters) as the key to
    ensure you cannot have two jobs running at the same time
    which would interfere with each other by trying to build
    the same target.

    To wait for a group of jobs to complete, call [wait group].
 *)

module type Key = sig
  type t
  val compare : t -> t -> int
  val to_string : t -> string
end

module type Jobs = sig
  type key
  type group

  val new_group : unit -> group
  (** Create a new empty jobs group. *)

  val start : group -> key -> (unit -> unit) -> unit
  (** [start group key f] submits a job to run in the background.
      The [key] ensures that two jobs with the same key cannot run
      at the same time (across all groups). *)

  val wait : group -> unit
  (** [wait group] waits for all of the jobs in the group to finish. *)
end

module Make (K : Key) : Jobs with type key = K.t
