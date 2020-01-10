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

open Utils

type 'a next = Job of 'a * (unit -> unit) | Complete | Not_ready

type 'a retire = 'a -> unit

type 'a to_string = 'a -> string

let run next_job retire_job string_of_job =
  (* Number of running threads <= Cmdline.nr_jobs. *)
  let running = ref 0 in

  (* Lock and condition for when a thread exits. *)
  let lock = Mutex.create () and cond = Condition.create () in

  (* If a job throws an exception it is saved here. *)
  let last_exn = ref None in

  (* This is the background thread which runs each job. *)
  let runner (job, f) =
    let exn = try f (); None with exn -> Some exn in

    Mutex.lock lock;
    (match exn with
     | None -> retire_job job
     | Some exn -> last_exn := Some exn
    );
    decr running;
    Condition.signal cond;
    Mutex.unlock lock
  in

  let rec loop () =
    if !last_exn = None then (
      match next_job () with
      | Complete -> ()
      | Not_ready ->
         assert (!running > 0);
         Cmdline.debug "%d/%d threads running, waiting for dependencies"
           !running (Cmdline.nr_jobs ());
         (* Wait for any running thread to finish. *)
         Condition.wait cond lock;
         loop ()
      | Job (job, f) ->
         incr running;
         ignore (Thread.create runner (job, f));
         (* If we've reached the limit on number of threads, wait
          * for any running thread to finish.
          *)
         while !running >= Cmdline.nr_jobs () do
           Condition.wait cond lock
         done;
         loop ()
    )
  in
  Mutex.lock lock;
  loop ();

  (* Wait for all jobs to complete. *)
  while !running > 0 do
    Cmdline.debug "%d/%d threads running, waiting for completion"
      !running (Cmdline.nr_jobs ());
    Condition.wait cond lock
  done;

  let exn = !last_exn in
  Mutex.unlock lock;

  (* Re-raise the saved exception from the job which failed. *)
  match exn with
  | None -> ()
  | Some exn -> raise exn
