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

module type Key = sig
  type t
  val compare : t -> t -> int
  val to_string : t -> string
end

module type Jobs = sig
  type key
  type group
  val new_group : unit -> group
  val start : group -> key -> (unit -> unit) -> unit
  val wait : group -> unit
  val stop_all : unit -> unit
end

module Make (K : Key) = struct
  type key = K.t

  type state = Waiting | Running | Done
  type job = {
    mutable state : state;
    f : unit -> unit;           (* The function to run the job. *)
  }

  type queue = {
    (* Lock preventing multiple jobs with the same key from
     * running at the same time.
     *)
    q_lock : Mutex.t;
    mutable q : job list;       (* List of jobs on this queue. *)
  }
  let new_queue () = { q_lock = Mutex.create (); q = [] }

  (* All of the shared state below is protected by this lock that
   * you must hold before using any of it.
   *)
  let lock = Mutex.create ()

  (* Jobs are queued on separate queues according to their key.
   * qs is a map of the key to the list of jobs in that queue.
   *)
  module Qs = Map.Make (K)
  let qs = ref Qs.empty

  (* Threads which are idle wait on this condition.  This is
   * signalled when:
   *  - a new job is added (idle threads may be able to run it)
   *  - a job finishes (idle threads may be able to run another
   *    job which has the same key as the one which finished)
   *)
  let idle = Condition.create ()

  (* Threads which are running or idle but NOT waiting.  This
   * starts as one because the main thread is running.  A thread
   * which is waiting is essentially blocking another job which
   * could run, so we should start a new thread.  A thread which
   * is idle on the other hand is not blocking anything from
   * running, it's idle because there is nothing that can be run.
   *
   * We aim to keep this <= Cmdline.nr_jobs.
   *)
  let ready = ref 1

  (* If stop_all is called, this is set to true and we stop
   * running new jobs.
   *)
  let stop = ref false

  (* The worker thread. *)
  let rec worker _ =
    let id = Thread.id (Thread.self ()) in
    Mutex.lock lock;
    incr ready;
    while not !stop && !ready <= Cmdline.nr_jobs () do
      (* See if there's any queue with a job which is ready to run. *)
      Cmdline.debug "thread %d: checking for a runnable queue" id;
      match get_runnable_queue () with
      | None ->
         (* Nothing that we can run, go idle.  This also drops
          * the lock so other threads can examine the queue.
          *)
         Cmdline.debug "thread %d: idle" id;
         Condition.wait idle lock;
      | Some q ->
         (* Note that q.q_lock is now held by this thread, and q.q
          * is non-empty.  Pick the job off the head of this queue.
          *)
         let job = List.hd q.q in
         q.q <- List.tl q.q;

         (* Run the job, dropping the main lock while running. *)
         job.state <- Running;
         Mutex.unlock lock;
         Cmdline.debug "thread %d: running job" id;
         job.f ();
         Cmdline.debug "thread %d: finished job" id;
         Mutex.lock lock;
         job.state <- Done;
         (* Since we have finished a job, it may be that other
          * idle threads could now run (if a job with the same
          * key is waiting).
          *)
         Mutex.unlock q.q_lock;
         Condition.broadcast idle
    done;
    decr ready;
    Mutex.unlock lock

  (* Check all the queues to see if there is any job which can run.
   * The lock must be held when calling this function.  This
   * locks the queue if it finds one.
   *)
  and get_runnable_queue () =
    try
      let qs = List.map snd (Qs.bindings !qs) in
      Some (List.find is_runnable_queue qs)
    with
      Not_found -> None

  (* Return true iff the queue contains jobs and no existing job
   * from this queue is already running.  This locks the queue
   * if it returns true.
   *)
  and is_runnable_queue = function
    | { q = [] } -> false
    | { q_lock } -> Mutex.try_lock q_lock

  (* A group is simply a list of jobs. *)
  type group = job list ref
  let new_group () = ref []

  (* Submit a new job. *)
  let start group key f =
    let id = Thread.id (Thread.self ()) in
    Cmdline.debug "thread %d: submitting new job" id;
    Mutex.lock lock;
    let job = { state = Waiting; f } in
    group := job :: !group;

    (* Put the job on the queue associated with this key. *)
    let q =
      try Qs.find key !qs
      with Not_found ->
        let q = new_queue () in (* Allocate a new queue for this key. *)
        qs := Qs.add key q !qs;
        q in
    q.q <- q.q @ [job];

    (* Wake up any idle threads. *)
    Condition.signal idle;
    Mutex.unlock lock

  (* Wait for all jobs in the group to be done. *)
  let rec wait group =
    let id = Thread.id (Thread.self ()) in
    Mutex.lock lock;
    while not (all_done group); do
      decr ready;
      (* Start more threads if fewer than nr_jobs threads are ready. *)
      let needed = Cmdline.nr_jobs () - !ready in
      if needed > 0 then
        ignore (Array.init needed (Thread.create worker));

      Cmdline.debug "thread %d: waiting for group to complete" id;
      Condition.wait idle lock;
      incr ready
    done;
    Mutex.unlock lock

  (* Test if all jobs in a group are done.  Note you must hold
   * the lock.
   *)
  and all_done group = List.for_all (fun { state } -> state = Done) !group

  let stop_all () =
    Mutex.lock lock;
    (* All threads will exit after running jobs if this is set. *)
    stop := true;
    while !ready > 1 do
      Condition.wait idle lock;
    done;
    Mutex.unlock lock

end
