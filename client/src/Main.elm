module Main exposing (main)

import Browser
import Browser.Events
import Discord
import Game
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode
import Pages.Playing as Playing
import Time
import Tween
import WebSocket as Ws



---- Model ----


type alias Model =
    { width : Int
    , height : Int
    , page : Page
    }


type Page
    = LoadingModel
    | HomeModel
        { auth : Discord.Auth
        , modal : Maybe { pinDraft : String }
        }
    | LobbyModel
        { auth : Discord.Auth
        , pin : String
        , players : Game.Store (List ( String, String ))
        , messages : List String
        }
    | PlayingModel Playing.Model



---- Init ----


init : () -> ( Model, Cmd Msg )
init () =
    ( { width = 0
      , height = 0
      , page =
            PlayingModel <|
                Playing.init
                    { access_token = "token"
                    , user =
                        { username = "user"
                        , discriminator = "0"
                        , id = ""
                        , avatar = Nothing
                        , global_name = Nothing
                        }
                    , expires = ""
                    }
                    0
                    0
      }
    , Cmd.none
    )



-- ( { width = 0, height = 0, page = LoadingModel }, Cmd.none )
---- Update ----


type Msg
    = ---- Generic ----
      WindowResized Int Int
    | WsReceive Ws.Receive
    | Error Json.Decode.Error
      ---- Loading ----
    | Authenticated Discord.Auth
      ---- Home ----
    | CreateLobbyRequest
    | JoinModalOpen
    | JoinModalPinChanged String
    | JoinLobbyRequest
      -- | CreateLobbySuccess
      -- | JoinLobbySuccess
      ---- Lobby ----
    | StartGameRequest
      ---- Playing ----
    | PlayingMsg Playing.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case ( message, model.page ) of
        ---- Generic messages ----
        ( WindowResized w h, PlayingModel m ) ->
            Tuple.mapBoth (\m2 -> { width = w, height = h, page = PlayingModel m2 }) (Cmd.map PlayingMsg) (Playing.update (Playing.WindowResized w h) m)

        ( WindowResized w h, p ) ->
            ( { width = w, height = h, page = p }, Cmd.none )

        ---- Loading ----
        ( Authenticated auth, LoadingModel ) ->
            ( { model | page = HomeModel { auth = auth, modal = Nothing } }, Cmd.none )

        ---- Home ----
        ( CreateLobbyRequest, HomeModel m ) ->
            ( model, Ws.send m.auth.user.id Ws.CreateLobbyRequest )

        ( JoinModalOpen, HomeModel m ) ->
            ( { model | page = HomeModel { m | modal = Just { pinDraft = "" } } }, Cmd.none )

        ( JoinModalPinChanged pin, HomeModel m ) ->
            ( { model | page = HomeModel { m | modal = Just { pinDraft = pin } } }, Cmd.none )

        ( JoinLobbyRequest, HomeModel m ) ->
            case m.modal of
                Nothing ->
                    ( model, Cmd.none )

                Just { pinDraft } ->
                    ( model, Ws.send m.auth.user.id (Ws.JoinLobbyRequest { pin = pinDraft }) )

        ( WsReceive (Ws.CreateLobbySuccess lobbyData), HomeModel m ) ->
            ( { model | page = LobbyModel { auth = m.auth, pin = lobbyData.pin, players = lobbyData.players, messages = [] } }, Cmd.none )

        ---- Lobby ----
        ( StartGameRequest, LobbyModel m ) ->
            ( model, Ws.send m.auth.user.id (Ws.StartGameRequest { pin = m.pin }) )

        ( WsReceive (Ws.StartGameFail reason), LobbyModel m ) ->
            ( { model | page = LobbyModel { m | messages = reason :: m.messages } }, Cmd.none )

        ( WsReceive (Ws.StartGameSuccess g), LobbyModel m ) ->
            ( { model
                | page =
                    PlayingModel
                        { auth = m.auth
                        , width = model.width
                        , height = model.height
                        , hands =
                            { s =
                                (case g.state of
                                    Game.Playing { hands } ->
                                        hands.s

                                    Game.Trading { hands } ->
                                        hands.s
                                )
                                    |> List.map (\c -> { pos = Tween.init { x = 0, y = 0 }, card = c, z = 0 })
                            , n =
                                List.repeat
                                    (case g.state of
                                        Game.Playing { hands } ->
                                            hands.n

                                        Game.Trading { hands } ->
                                            hands.n
                                    )
                                    Nothing
                                    |> List.map (\c -> { pos = Tween.init { x = 0, y = 0 }, card = c, z = 0 })
                            , w =
                                List.repeat
                                    (case g.state of
                                        Game.Playing { hands } ->
                                            hands.w

                                        Game.Trading { hands } ->
                                            hands.w
                                    )
                                    ()
                                    |> List.map (\c -> { pos = Tween.init { x = 0, y = 0 }, card = c, z = 0 })
                            , e =
                                List.repeat
                                    (case g.state of
                                        Game.Playing { hands } ->
                                            hands.e

                                        Game.Trading { hands } ->
                                            hands.e
                                    )
                                    ()
                                    |> List.map (\c -> { pos = Tween.init { x = 0, y = 0 }, card = c, z = 0 })
                            }
                        , table = []
                        , selected = []
                        , mouse = { x = 0, y = 0 }
                        , selectionPos = { x = 0, y = 0 }
                        }
              }
            , Cmd.none
            )

        ---- Delegate to page specific update function ----
        ( PlayingMsg msg, PlayingModel m ) ->
            let
                ( model2, cmd ) =
                    Playing.update msg m
            in
            ( { model | page = PlayingModel model2 }, Cmd.map PlayingMsg cmd )

        ---- Received a message intended for a different page? Ignore it!  ----
        ---- Received a message intended for a different page? Ignore it!  ----
        ( Error e, _ ) ->
            ( Debug.log (Json.Decode.errorToString e) model, Cmd.none )

        _ ->
            ( model, Cmd.none )



---- Subscriptions ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize WindowResized
        , case model.page of
            LoadingModel ->
                Discord.authenticated Authenticated

            HomeModel _ ->
                Ws.receive WsReceive Error

            LobbyModel _ ->
                Ws.receive WsReceive Error

            PlayingModel _ ->
                Time.every 16 Playing.Tick |> Sub.map PlayingMsg
        ]



---- View ----


view : Model -> Html Msg
view model =
    case model.page of
        LoadingModel ->
            Html.div [] [ Html.text "Loading..." ]

        HomeModel m ->
            Html.div []
                [ Html.h1 [] [ Html.text "Hello!" ]
                , Html.div [] [ Html.text (Debug.toString m.auth) ]
                , Html.button
                    [ HtmlE.onClick CreateLobbyRequest ]
                    [ Html.text "Create lobby" ]
                , Html.button
                    [ HtmlE.onClick JoinModalOpen ]
                    [ Html.text "Join lobby" ]
                , case m.modal of
                    Nothing ->
                        Html.div [] []

                    Just { pinDraft } ->
                        Html.div []
                            [ Html.input
                                [ HtmlA.placeholder "Pin"
                                , HtmlE.onInput JoinModalPinChanged
                                , HtmlA.value pinDraft
                                ]
                                []
                            , Html.button [ HtmlE.onClick JoinLobbyRequest ] [ Html.text "Join" ]
                            ]
                ]

        LobbyModel m ->
            Html.div []
                [ Html.h1 [] [ Html.text "Lobby" ]
                , Html.p [] [ Html.text m.pin ]
                , Html.p [] [ Html.text (Debug.toString m.players) ]
                , Html.button [ HtmlE.onClick StartGameRequest ] [ Html.text "Play" ]
                , Html.ul [] (m.messages |> List.map (\msg -> Html.li [] [ Html.text msg ]))
                ]

        PlayingModel m ->
            Playing.view m |> Html.map PlayingMsg



---- Main ----


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
