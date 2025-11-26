open Base
open Guandan

type t = (string, game, String.comparator_witness) Map.t

and game =
  | Lobby of {
      clients :
        ( string,
          Dream.websocket * Discord.user,
          String.comparator_witness )
        Map.t;
          (* client_id <-> websocket * (Discord) user {id, username, avatar ...} *)
      players : string list Player.store; (* list of client_id *)
    }
  | Game of {
      clients :
        ( string,
          Dream.websocket * Discord.user,
          String.comparator_witness )
        Map.t;
      players : string list Player.store;
      state : Game.server_t;
    }

let init : t = Map.empty (module String)
