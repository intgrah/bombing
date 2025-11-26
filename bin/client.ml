open Base
open Guandan
open Types
open Ppx_yojson_conv_lib.Yojson_conv

type receive_with_user_id = string * receive

and receive =
  | Create_lobby_request
  | Join_lobby_request of { pin : string }
  | Start_game_request of { pin : string }
  | Game_action of { pin : string; action : Game.action }
[@@deriving of_yojson]

type send =
  | Create_lobby_success of lobby_data
  | Join_lobby_success of lobby_data
  | Lobby_update of lobby_data
  | Join_lobby_fail of [ `No_such_lobby | `In_progress | `Already_joined ]
  | Start_game_success of game_data
  | Start_game_fail of
      [ `No_such_lobby | `In_progress | `Positions_not_fulfilled ]

and lobby_data = {
  pin : string;
  players : username_and_avatar list Player.store;
}

and game_data = {
  pin : string;
  players : username_and_avatar list Player.store;
  state : Game.client_t;
}

and username_and_avatar = { username : string; avatar : string }

and hand_proj = { s : Card.t list; n : int; w : int; e : int }
[@@deriving yojson_of]

let send (stc : send) (socket : Dream.websocket) : unit Lwt.t =
  stc |> yojson_of_send |> Yojson.Safe.to_string |> Dream.send socket

let broadcast (stc : send) : Dream.websocket list -> unit Lwt.t =
  Lwt_list.iter_p (send stc)
