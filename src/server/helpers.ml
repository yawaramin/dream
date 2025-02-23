(* This file is part of Dream, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/dream.

   Copyright 2021 Anton Bachin *)



module Formats = Dream_pure.Formats
module Message = Dream_pure.Message
module Status = Dream_pure.Status
module Stream = Dream_pure.Stream



let client_field =
  Message.new_field
    ~name:"dream.client"
    ~show_value:(fun client -> client)
    ()

(* TODO What should be reported when the client address is missing? This is a
   sign of local testing. *)
let client request =
  match Message.field request client_field with
  | None -> "127.0.0.1:0"
  | Some client -> client

let set_client request client =
  Message.set_field request client_field client



let https_field =
  Message.new_field
    ~name:"dream.https"
    ~show_value:string_of_bool
    ()

let https request =
  match Message.field request https_field with
  | Some true -> true
  | _ -> false

let set_https request https =
  Message.set_field request https_field https



let request ~client ~method_ ~target ~https ~version ~headers server_stream =
  let request =
    Message.request
      ~method_ ~target ~version ~headers Stream.null server_stream in
  set_client request client;
  set_https request https;
  request

let request_with_body ?method_ ?target ?version ?headers body =
  Message.request
    ?method_ ?target ?version ?headers Stream.null (Stream.string body)



let html ?status ?code ?headers body =
  let response =
    Message.response ?status ?code ?headers (Stream.string body) Stream.null in
  Message.set_header response "Content-Type" Formats.text_html;
  Lwt.return response

let json ?status ?code ?headers body =
  let response =
    Message.response ?status ?code ?headers (Stream.string body) Stream.null in
  Message.set_header response "Content-Type" Formats.application_json;
  Lwt.return response

let response_with_body ?status ?code ?headers body =
  Message.response ?status ?code ?headers (Stream.string body) Stream.null

let respond ?status ?code ?headers body =
  Message.response ?status ?code ?headers (Stream.string body) Stream.null
  |> Lwt.return

(* TODO Actually use the request and extract the site prefix. *)
let redirect ?status ?code ?headers _request location =
  let status = (status :> Status.redirection option) in
  let status =
    match status, code with
    | None, None -> Some (`See_Other)
    | _ -> status
  in
  let response =
    Message.response ?status ?code ?headers Stream.empty Stream.null in
  Message.set_header response "Location" location;
  Lwt.return response

let stream ?status ?code ?headers callback =
  let reader, writer = Stream.pipe () in
  let client_stream = Stream.stream reader Stream.no_writer
  and server_stream = Stream.stream Stream.no_reader writer in
  let response =
    Message.response ?status ?code ?headers client_stream server_stream in
  (* TODO Should set up an error handler for this. YES. *)
  (* TODO Make sure the request id is propagated to the callback. *)
  Lwt.async (fun () -> callback response);
  Lwt.return response

let websocket_field =
  Message.new_field
    ~name:"dream.websocket"
    ~show_value:(Printf.sprintf "%b")
    ()

let is_websocket response =
  match Message.field response websocket_field with
  | Some true -> true
  | _ -> false

(* TODO Mark the request as a WebSocket request for HTTP. *)
let websocket ?headers callback =
  let in_reader, in_writer = Stream.pipe ()
  and out_reader, out_writer = Stream.pipe () in
  let client_stream = Stream.stream out_reader in_writer
  and server_stream = Stream.stream in_reader out_writer in
  let response =
    Message.response
      ~status:`Switching_Protocols ?headers client_stream server_stream in
  Message.set_field response websocket_field true;
  (* TODO Make sure the request id is propagated to the callback. *)
  (* TODO Close the WwbSocket on leaked exceptions, etc. *)
  Lwt.async (fun () -> callback response);
  Lwt.return response

let empty ?headers status =
  respond ?headers ~status ""

let not_found _ =
  respond ~status:`Not_Found ""



let read message =
  Message.read (Message.server_stream message)

let write ?kind message chunk =
  Message.write ?kind (Message.server_stream message) chunk

let flush message =
  Message.flush (Message.server_stream message)
