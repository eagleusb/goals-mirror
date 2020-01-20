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

(** This module manages parallel jobs. *)

type 'a next = Job of 'a * (unit -> unit) | Complete | Not_ready

val run : (unit -> 'a next) -> ('a -> unit) -> ('a -> string) -> unit
(** [run next_job retire_job to_string] runs jobs in parallel.

    [next_job] is called to pick the next available job.
    [retire_job] is called when a job finishes successfully.
    [to_string] is called if we need to print the job name.

    If [next_job] returns [Job f] then that function is started
    (usually in a thread if -j N > 1).

    If [next_job] returns [Complete] then [run] waits until
    all parallel jobs are then returns.

    If [next_job] returns [Not_ready] then [next_job] will be
    called again after a little while.

    If any job throws an exception then the exception will be
    reraised by [run], usually causing goals to exit with an error.
    The exception is delayed until all currently running jobs
    finish, but no new jobs will be started during this time. *)
