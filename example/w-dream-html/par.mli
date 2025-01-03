type t
(** The parallel task runner. *)

type env = <
  backend_id : string;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  cwd : Eio.Fs.dir_ty Eio.Path.t; debug : Eio.Debug.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  mono_clock : Eio.Time.Mono.ty Eio.Resource.t;
  net : [ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t;
  par : t;
  process_mgr : Eio_unix__.Process.mgr_ty Eio.Resource.t;
  secure_random : Eio.Flow.source_ty Eio.Resource.t;
  stderr : Eio_unix.sink_ty Eio.Resource.t;
  stdin : Eio_unix.source_ty Eio.Resource.t;
  stdout : Eio_unix.sink_ty Eio.Resource.t
>
(** An Eio env with the domain manager omitted and the parallel task runner added. *)

val run : (env -> unit) -> unit
(** [run fn] is the multicore equivalent of [Eio_main.run fn]. It automatically
    starts running [fn] on all available cores. Also, it makes available a
    parallel task runner that can distribute the work across all available cores. *)

val exec : < par : t; .. > -> (unit -> 'a) -> 'a array Eio.Promise.t
(** [exec env fn] is a an array of results of running [fn] in the parallel task
    runner provided by [env]. It is expected that the caller uses [num_domains]
    and [id] to calculate the portion of the work that should be given to each
    domain. *)

val num_domains : int
(** [num_domains] is just [Domain.recommended_domain_count ()]. *)

val id : unit -> int
(** [id ()] is the integer ID of the current domain, starting from 0.

    Note that domain ID 0 is never given any tasks to run by [exec]; it is kept
    free for I/O work. *)

val sum : < par : t; .. > -> float array -> float Eio.Promise.t
(** [sum env floats] is the sum of the [floats] calculated by distributing
    portions of the work across all domains provided by [env]. *)
