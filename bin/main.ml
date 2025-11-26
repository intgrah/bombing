open Base
open Guandan

(* type foo = Baz of int list | Foo of { bar : int; baz : string }
[@@deriving sexp]

let () =
  Stdio.print_endline @@ Sexp.to_string @@ yojson_of_foo
  @@ Foo { bar = 0; baz = "baz" };
  Stdio.print_endline @@ Sexp.to_string @@ yojson_of_foo @@ Baz [ 1; 3; 5 ];
  Stdio.print_endline @@ Sexp.to_string
  @@ Player.yojson_of_store (List.yojson_of_t String.yojson_of_t)
  @@ Player.Store ([ "e" ], [ ""; "" ], [ "" ], [ ""; "" ]) *)

let () =
  Stdio.print_endline @@ Yojson.Safe.to_string
  @@ Types.Rank.yojson_of_t Types.Rank.Two

let state : Model.t ref = ref Model.init

let projection
    (clients :
      ( string,
        Dream.websocket * Discord.user,
        Base.String.comparator_witness )
      Base.Map.t) :
    string list Player.store -> Client.username_and_avatar list Player.store =
  let project (user_ids : string list) : Client.username_and_avatar list =
    List.filter_map user_ids ~f:(Map.find clients)
    |> List.map ~f:(fun ((_, user) : Dream.websocket * Discord.user) ->
           ({ username = user.username; avatar = "none" }
             : Client.username_and_avatar))
  in
  function
  | { a; b; c; d } ->
      { a = project a; b = project b; c = project c; d = project d }

let update ~(socket : Dream.websocket) ~(user : Discord.user)
    (msg : Client.receive) (model : Model.t) : Model.t * unit Lwt.t =
  match msg with
  | Create_lobby_request ->
      let pin : string = Random.int 1000000 |> Int.to_string in
      let clients' = Map.singleton (module String) user.id (socket, user) in
      let players' : string list Player.store =
        { a = [ user.id ]; b = [ user.id ]; c = [ user.id ]; d = [ user.id ] }
      in
      let game' : Model.game =
        Model.Lobby { clients = clients'; players = players' }
      in
      let model' = Map.add_exn model ~key:pin ~data:game' in
      ( model',
        Client.send
          (Create_lobby_success { pin; players = projection clients' players' })
          socket )
      (* Generate a random pin.
         Create a new set of clients with just one client.
         Create a new set of players with A played by the client.
         Project *)
  | Join_lobby_request { pin } -> (
      match Map.find model pin with
      | None -> (model, Client.send (Join_lobby_fail `No_such_lobby) socket)
      | Some (Game _) ->
          (model, Client.send (Join_lobby_fail `In_progress) socket)
      | Some (Lobby lobby) -> (
          match Map.add lobby.clients ~key:user.id ~data:(socket, user) with
          | `Duplicate ->
              (model, Client.send (Join_lobby_fail `Already_joined) socket)
          | `Ok clients' ->
              let players' : string list Player.store =
                match lobby.players with
                | { a = []; b; c; d } -> { a = [ user.id ]; b; c; d }
                | { a; b = []; c; d } -> { a; b = [ user.id ]; c; d }
                | { a; b; c = []; d } -> { a; b; c = [ user.id ]; d }
                | { a; b; c; d = [] } -> { a; b; c; d = [ user.id ] }
                | { a; b; c; d } -> { a = user.id :: a; b; c; d }
              in
              let game' : Model.game =
                Lobby { clients = clients'; players = players' }
              in
              let model' = Map.set model ~key:pin ~data:game' in
              let usernames_and_avatars = projection clients' players' in
              let lobby_data : Client.lobby_data =
                { pin; players = usernames_and_avatars }
              in
              ( model',
                let%lwt () =
                  Client.send (Join_lobby_success lobby_data) socket
                in
                lobby.clients |> Map.data |> List.map ~f:fst
                |> Lwt_list.iter_p (Client.send (Lobby_update lobby_data)) )))
  | Start_game_request { pin } -> (
      match Map.find model pin with
      | None -> (model, Client.send (Start_game_fail `No_such_lobby) socket)
      | Some (Game _) ->
          (model, Client.send (Start_game_fail `In_progress) socket)
      | Some (Lobby lobby) -> (
          match lobby.players with
          | { a = _ :: _; b = _ :: _; c = _ :: _; d = _ :: _ } ->
              let game_state' : Game.server_t = Game.new_game () in
              let game' : Model.game =
                Game
                  {
                    clients = lobby.clients;
                    players = lobby.players;
                    state = game_state';
                  }
              in
              let model' = Map.set model ~key:pin ~data:game' in
              ( model',
                Lwt_list.iter_p Fn.id
                  (* (Start_game_success { pin; client_id; state = game_state' }) *)
                  (Player.mapi lobby.players ~f:(fun p ids ->
                       List.map ids ~f:(fun id ->
                           let ws, _ = Map.find_exn lobby.clients id in
                           Client.send
                             (Start_game_success
                                {
                                  pin;
                                  players =
                                    projection lobby.clients lobby.players;
                                  state = Game.project p game_state';
                                })
                             ws))
                  |> Player.to_list |> List.concat) )
              (* (Map.data lobby.clients |> List.map ~f:fst) ) *)
          | _ ->
              ( model,
                Client.send (Start_game_fail `Positions_not_fulfilled) socket ))
      )
  | _ -> (model, Lwt.return_unit)

let main () =
  let ws_server (_ : Dream.request) (socket : Dream.websocket) : unit Lwt.t =
    (* match Dream.query req "token" with
    | None -> Dream.close_websocket socket
    | Some access_token -> (
        match%lwt Discord.get_user access_token with
        | None -> Dream.close_websocket socket
        | Some user ->
            Dream.log "New client: %s\n" user.username; *)
    let user : Discord.user = { id = "foo"; username = "user"; avatar = "" } in
    let rec loop () =
      match%lwt Dream.receive socket with
      | None -> Dream.close_websocket socket
      | Some msg_str -> (
          try
            let json = Yojson.Safe.from_string msg_str in
            let _user_id, msg = Client.receive_with_user_id_of_yojson json in
            let state', cmd = update ~socket ~user msg !state in
            state := state';
            let%lwt () = cmd in
            Dream.log "  %s: %s\n" user.username msg_str;
            loop ()
          with Yojson.Json_error err ->
            Dream.log "  %s: ERROR %s\n" err msg_str;
            loop ())
    in
    loop ()
    (* ) *)
  in
  Dream.router
    [
      Dream.post "/api/token" Discord.token_handler;
      Dream.get "/api/socket" (fun req -> Dream.websocket (ws_server req));
    ]
  |> Dream.logger |> Dream.run ~port:3001

let () = main ()
