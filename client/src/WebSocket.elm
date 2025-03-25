port module WebSocket exposing (Receive(..), Send(..), decode, encode, receive, send)

import Card
import Game
import Json.Decode as D
import Json.Encode as E



---- Internal JavaScript WebSocket ports ----
{-
   JavaScript ports only allow certain types, like Int, String or JSON.
   JSON is handy, since we can encode and decode our Elm types to JSON.

   These ports communicate with a WebSocket.

   NOTE: "E.Value" actually means JSON (its full name would be Json.Encode.Value)!
-}


port internalSend : String -> Cmd msg


port internalReceive : (String -> msg) -> Sub msg



---- Wrapper ports ----
{-
   These "ports" are just wrappers around the JavaScript ports,
   with the encoding and decoding already done!
-}


send : String -> Send -> Cmd msg
send userId msg =
    E.list identity [ E.string userId, encode msg ] |> E.encode 0 |> internalSend


receive : (Receive -> msg) -> (D.Error -> msg) -> Sub msg
receive onSuccess onError =
    internalReceive
        (\val ->
            case D.decodeString decode (Debug.log (Debug.toString val) val) of
                Ok stc ->
                    Debug.log (Debug.toString stc)
                        onSuccess
                        stc

                Err e ->
                    onError e
        )



---- Client to Server messages ----


type Send
    = CreateLobbyRequest
    | JoinLobbyRequest { pin : String }
    | StartGameRequest { pin : String }
    | GameAction { pin : String, action : Game.Action }



---- Server to Client messages ----


type Receive
    = CreateLobbySuccess LobbyData
    | JoinLobbySuccess LobbyData
    | LobbyUpdate LobbyData
    | JoinLobbyFail String
    | StartGameSuccess GameData
    | StartGameFail String


type alias LobbyData =
    { pin : String
    , players : Game.Store (List ( String, String ))
    }


type alias GameData =
    { pin : String
    , players : Game.Store (List ( String, String ))
    , state : Game.State
    }



---- Client to Server JSON encoders ----


encode : Send -> E.Value
encode msg =
    case msg of
        CreateLobbyRequest ->
            E.list identity [ E.string "Create_lobby_request" ]

        JoinLobbyRequest { pin } ->
            E.list identity [ E.string "Join_lobby_request", E.object [ ( "pin", E.string pin ) ] ]

        StartGameRequest { pin } ->
            E.list identity [ E.string "Start_game_request", E.object [ ( "pin", E.string pin ) ] ]

        GameAction { pin, action } ->
            E.list identity
                [ E.string "Game_action"
                , E.list identity [ E.string "pin", E.string pin ]
                , E.list identity
                    [ E.string "action"
                    , case action of
                        Game.Revolt ->
                            E.string "Revolt"

                        Game.Trade card ->
                            E.list identity [ E.string "Trade", Card.encode card ]

                        Game.Play cards ->
                            E.list identity [ E.string "Play", E.list identity <| List.map Card.encode cards ]

                        Game.Pass ->
                            E.string "Pass"
                    ]
                ]



---- Server to Client JSON decoders ----


decode : D.Decoder Receive
decode =
    D.index 0 D.string
        |> D.andThen
            (\variant ->
                case variant of
                    "Create_lobby_success" ->
                        D.index 1 <| D.map CreateLobbySuccess decodeLobbyData

                    "Join_lobby_success" ->
                        D.index 1 <| D.map JoinLobbySuccess decodeLobbyData

                    "Lobby_update" ->
                        D.index 1 <| D.map LobbyUpdate decodeLobbyData

                    "Join_lobby_fail" ->
                        D.index 1 <| D.map JoinLobbyFail (D.index 0 D.string)

                    "Start_game_success" ->
                        D.index 1 <| D.map StartGameSuccess decodeGameData

                    "Start_game_fail" ->
                        D.index 1 <| D.map StartGameFail (D.index 0 D.string)

                    _ ->
                        Debug.log variant D.fail "Unknown variant"
            )


decodeLobbyData : D.Decoder LobbyData
decodeLobbyData =
    D.map2
        LobbyData
        (D.field "pin" D.string)
        (D.field "players" <| decodeStore <| D.list decodeUsernameAvatar)


decodeGameData : D.Decoder GameData
decodeGameData =
    D.map3
        GameData
        (D.field "pin" D.string)
        (D.field "players" <| decodeStore <| D.list decodeUsernameAvatar)
        (D.field "state" decodeGameState)


decodeGameState : D.Decoder Game.State
decodeGameState =
    D.index 0 D.string
        |> D.andThen
            (\variant ->
                case variant of
                    "Playing" ->
                        D.index 1 <|
                            D.map2 (\hands level -> Game.Playing { hands = hands, level = level })
                                (D.field "hands" decodeHands)
                                (D.field "level" <|
                                    D.map3 (\ac bd who -> { ac = ac, bd = bd, who = who })
                                        (D.field "ac" Card.decodeRank)
                                        (D.field "bd" Card.decodeRank)
                                        (D.field "who" Game.decodeTeam)
                                )

                    "Trading" ->
                        D.index 1 <|
                            D.map4
                                (\hands level positions traded ->
                                    Game.Trading { hands = hands, level = level, positions = positions, remaining = traded }
                                )
                                (D.field "hands" <| decodeHands)
                                (D.field "level" <|
                                    D.map3 (\ac bd who -> { ac = ac, bd = bd, who = who })
                                        (D.field "ac" Card.decodeRank)
                                        (D.field "bd" Card.decodeRank)
                                        (D.field "who" Game.decodeTeam)
                                )
                                (D.field "positions" <| decodeStore Game.decodePosition)
                                (D.field "traded" <| decodeStore D.int)

                    _ ->
                        Debug.log variant D.fail "Unknown game state variant"
            )


decodeHands : D.Decoder Game.Hands
decodeHands =
    D.map4 Game.Hands
        (D.field "s" <| D.list Card.decode)
        (D.field "e" D.int)
        (D.field "n" D.int)
        (D.field "w" D.int)


decodeStore : D.Decoder a -> D.Decoder (Game.Store a)
decodeStore d =
    D.index 0 D.string
        |> D.andThen
            (\variant2 ->
                case variant2 of
                    "Store" ->
                        D.map4 Game.Store
                            (D.index 1 d)
                            (D.index 2 d)
                            (D.index 3 d)
                            (D.index 4 d)

                    _ ->
                        D.fail "Unknown variant of store"
            )


decodeUsernameAvatar : D.Decoder ( String, String )
decodeUsernameAvatar =
    D.map2 Tuple.pair
        (D.field "username" D.string)
        (D.field "avatar" D.string)
