type task = Task : (unit -> 'a) * ('a -> unit) -> task
type t = task Eio.Stream.t array

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

let id () = (Domain.self () :> int)
let workers env = Array.length env#par

let mkenv streams prev = object
  method stdin = prev#stdin
  method stdout = prev#stdout
  method stderr = prev#stderr
  method net = prev#net
  method process_mgr = prev#process_mgr
  method clock = prev#clock
  method mono_clock = prev#mono_clock
  method fs = prev#fs
  method cwd = prev#cwd
  method secure_random = prev#secure_random
  method debug = prev#debug
  method backend_id = prev#backend_id
  method par = streams
end

let run domain = match Domain.recommended_domain_count () with
  | 1 -> Eio_main.run (fun env -> domain (mkenv [||] env))
  | num_domains ->
    let streams = Array.init (pred num_domains) (fun _ -> Eio.Stream.create 32) in
    Eio_main.run @@ fun env ->
    let base () = domain (mkenv streams env) in
    let worker _ () = Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
      Eio.Fiber.both base (fun () ->
        while true do
          let Task (f, cb) = Eio.Stream.take streams.(pred (id ())) in
          cb (f ())
        done))
    in
    let fibers = base :: List.init (pred num_domains) worker in
    Eio.Fiber.all fibers

let exec env f =
  let num_workers = workers env in
  let promises = Array.init num_workers (fun idx ->
    let p, r = Eio.Promise.create () in
    Eio.Stream.add env#par.(idx) (Task (f, Eio.Promise.resolve r));
    p)
  in
  let dynarr = Dynarray.create () in
  Eio.Fiber.all (List.init num_workers (fun idx () ->
    Dynarray.add_last dynarr (Eio.Promise.await promises.(idx))));
  Dynarray.to_array dynarr

let sum arr pos len =
  let res = ref 0.
  and carry = ref 0. in
  for idx = pos to pos + len - 1 do
    let arr_idx = arr.(idx) -. !carry in
    let temp = !res +. arr_idx in
    carry := temp -. !res -. arr_idx;
    res := temp
  done;
  !res

let sum env arr =
  let len = Array.length arr
  and num_workers = workers env in
  if len < num_workers then
    sum arr 0 len
  else
    let len1 = len / num_workers in
    let len0 = len1 + len mod num_workers in
    let arr = exec env (fun () ->
      let idx = pred (id ()) in
      let pos = if idx = 1 then len0 else idx * len1 + len0 - len1 in
      sum arr pos (if idx = 0 then len0 else len1))
    in
    sum arr 0 num_workers
