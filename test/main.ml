(* Copyright 2017 Aesthetic Integration, Ltd.                               *)
(*                                                                          *)
(* Author: Matt Bray (matt@aestheticintegration.com)                        *)
(*                                                                          *)
(* Licensed under the Apache License, Version 2.0 (the "License");          *)
(* you may not use this file except in compliance with the License.         *)
(* You may obtain a copy of the License at                                  *)
(*                                                                          *)
(*     http://www.apache.org/licenses/LICENSE-2.0                           *)
(*                                                                          *)
(* Unless required by applicable law or agreed to in writing, software      *)
(* distributed under the License is distributed on an "AS IS" BASIS,        *)
(* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *)
(* See the License for the specific language governing permissions and      *)
(* limitations under the License.                                           *)
(*                                                                          *)

open OUnit2

let assert_string_equal s1 s2 =
  assert_equal
    ~printer:(fun s -> s)
    s1
    s2

let engineio_parser_suite =
  let open Engineio_client in

  let assert_packets_equal packets1 packets2 =
    assert_equal
      ~printer:(fun packets ->
          String.concat "; "
            (List.map Packet.string_of_t packets))
      packets1
      packets2
  in

  let test_decode_payload_as_binary test_ctxt =
    assert_packets_equal
      [ (Packet.OPEN, Packet.P_None) ]
      (Parser.decode_payload_as_binary "\000\001\2550")
  in

  let test_decode_payload_two_packets test_ctxt =
    assert_packets_equal
      [ (Packet.MESSAGE, Packet.P_String "0")
      ; (Packet.PONG, Packet.P_None)
      ]
      (Parser.decode_payload_as_binary "\000\002\25540\000\001\2553")
  in

  let test_encode_payload test_ctxt =
    assert_string_equal
      "\000\006\2552probe"
      (Parser.encode_payload [(Packet.PING, Packet.P_String "probe")])
  in

  [ "Engineio_client.Parser.decode_payload_as_binary" >:: test_decode_payload_as_binary
  ; "Engineio_client.Parser.decode_payload_two_packets" >:: test_decode_payload_two_packets
  ; "Engineio_client.Parser.encode_payload" >:: test_encode_payload
  ]

let socketio_parser_suite =
  let open Socketio_client in

  let string_of_packet packet =
    let brief = Packet.string_of_t packet in
    Printf.sprintf "%s: %s"
      brief
      (match packet with
       | Packet.EVENT (_, args, _, nsp) ->
         Printf.sprintf "%s%s"
           (nsp
            |> Eio_util.Option.value_map ~default:""
              ~f:(fun ns -> Printf.sprintf "nsp:%s " ns))
           (args
            |> List.map Yojson.Basic.to_string
            |> String.concat ", ")
       | _ -> "_")
  in

  let assert_packet_equal packet1 packet2 =
    assert_equal
      ~printer:string_of_packet
      packet1
      packet2
  in

  let test_decode_connect test_ctxt =
    assert_packet_equal
      (Packet.CONNECT None)
      (Parser.decode_packet "0")
  in

  let test_decode_connect_namespace test_ctxt =
    assert_packet_equal
      (Packet.CONNECT (Some "/namespace"))
      (Parser.decode_packet "0/namespace")
  in

  let test_decode_event test_ctxt =
    assert_packet_equal
      (Packet.EVENT ("my_event", [], None, None))
      (Parser.decode_packet "2[\"my_event\"]")
  in

  let test_decode_event_ack test_ctxt =
    assert_packet_equal
      (Packet.EVENT ("my_event", [], Some 1, None))
      (Parser.decode_packet "21[\"my_event\"]")
  in

  let test_decode_event_args test_ctxt =
    assert_packet_equal
      (Packet.EVENT ("my_event", [`String "arg_one"; `Assoc [("key", `String "val")]], Some 1, None))
      (Parser.decode_packet "21[\"my_event\",\"arg_one\",{\"key\":\"val\"}]")
  in

  let test_decode_event_namespace test_ctxt =
    assert_packet_equal
      (Packet.EVENT ("my_event", [], None, Some "/a-namespace"))
      (Parser.decode_packet "2/a-namespace,[\"my_event\"]")
  in

  let test_decode_event_namespace_ack test_ctxt =
    assert_packet_equal
      (Packet.EVENT ("my_event", [`String "my_arg"], Some 3, Some "/a-namespace"))
      (Parser.decode_packet "2/a-namespace,3[\"my_event\",\"my_arg\"]")
  in

  let test_decode_ack test_ctxt =
    assert_packet_equal
      (Packet.ACK ([`String "arg_one"], 1, None))
      (Parser.decode_packet "31[\"arg_one\"]")
  in

  let test_decode_error test_ctxt =
    assert_packet_equal
      (Packet.ERROR "This is an error.")
      (Parser.decode_packet "4This is an error.")
  in

  let test_encode_event_namespace test_ctxt =
    assert_string_equal
      "2/a-namespace,[\"my_event\"]"
      (Packet.event "my_event" [] ~namespace:"/a-namespace"
       |> Parser.encode_packet)
  in

  let test_encode_event_namespace_ack test_ctxt =
    assert_string_equal
      "2/a-namespace,3[\"my_event\"]"
      (Packet.event "my_event" [] ~ack:3 ~namespace:"/a-namespace"
       |> Parser.encode_packet)
  in

  let test_encode_ack_namespace test_ctxt =
    assert_string_equal
      "3/a-namespace,0[\"my_arg\"]"
      (Packet.ack 0 [`String "my_arg"] ~namespace:"/a-namespace"
       |> Parser.encode_packet)
  in

  [ "Socketio_client.Parser.decode_packet CONNECT" >:: test_decode_connect
  ; "Socketio_client.Parser.decode_packet CONNECT with namespace" >:: test_decode_connect_namespace
  ; "Socketio_client.Parser.decode_packet EVENT" >:: test_decode_event
  ; "Socketio_client.Parser.decode_packet EVENT with ack" >:: test_decode_event_ack
  ; "Socketio_client.Parser.decode_packet EVENT with args" >:: test_decode_event_args
  ; "Socketio_client.Parser.decode_packet EVENT with namespace" >:: test_decode_event_namespace
  ; "Socketio_client.Parser.decode_packet EVENT with namespace and ack" >:: test_decode_event_namespace_ack
  ; "Socketio_client.Parser.decode_packet ACK" >:: test_decode_ack
  ; "Socketio_client.Parser.decode_packet ERROR" >:: test_decode_error
  ; "Socketio_client.Parser.encode_packet EVENT with namespace" >:: test_encode_event_namespace
  ; "Socketio_client.Parser.encode_packet EVENT with namespace and ack" >:: test_encode_event_namespace_ack
  ; "Socketio_client.Parser.encode_packet ACK with namespace" >:: test_encode_ack_namespace
  ]

module Mock_Transport : Engineio_client.Transport_S = struct
  open Engineio_client

  module Polling = struct

    let name = "polling"

    type poll_error =
      { code : int
      ; body : string
      }
    exception Polling_exception of poll_error
  end

  module WebSocket = struct
    let name = "websocket"
  end

  type transport =
    | Polling
    | WebSocket

  type t =
      { transport : transport
      ; ready_state : ready_state
      ; packets : Packet.t Lwt_stream.t
      ; push_packet : Packet.t option -> unit
      }

  let string_of_t : t -> string =
    fun t ->
      match t.transport with
      | Polling -> Polling.name
      | WebSocket -> WebSocket.name

  let ready_state : t -> ready_state =
    fun t ->
      t.ready_state

  let packet_stream : t -> Packet.t Lwt_stream.t =
    fun t ->
      t.packets

  let create : transport -> Uri.t -> t =
    fun transport uri ->
      let packets, push_packet = Lwt_stream.create () in
      { transport =
          transport
      ; ready_state =
          Closed
      ; packets =
          packets
      ; push_packet =
          push_packet
      }

  let create_polling : Uri.t -> t =
    fun uri ->
      create Polling uri

  let create_websocket : Uri.t -> t =
    fun uri ->
      create WebSocket uri

  let open_ : t -> t Lwt.t =
    fun t -> Lwt.return t

  let write : t -> Packet.t list -> unit Lwt.t =
    fun t packets ->
      Lwt.return_unit

  let receive : t -> unit Lwt.t =
    fun t ->
      Lwt.return_unit

  let close : t -> t Lwt.t =
    fun t ->
      Lwt.return t

  let push_packet : t -> Packet.t option -> unit =
    fun t packet ->
      t.push_packet packet

  let on_open : t -> Parser.handshake -> t =
    fun t handshake ->
      t

  let on_close : t -> t =
    fun t ->
      t
end

module Mock_Socket = Engineio_client.Make_Socket(Mock_Transport)


let () =
  run_test_tt_main
    ("suite" >:::
     List.concat
       [ engineio_parser_suite
       ; socketio_parser_suite
       ])
