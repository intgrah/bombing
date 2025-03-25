module Pages.Playing exposing (Model, Msg(..), init, update, view)

import Card exposing (Card(..), Joker(..), Rank(..), Suit(..))
import Discord
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events.Extra.Mouse as HtmlE
import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Time
import Transform
import Tween exposing (Tween)
import Vector exposing (Vector)



---- Model ----


type alias Container =
    { s : Int -> Int -> Vector
    , i : Vector -> Int -> Int
    }


without : List a -> List ( a, List a )
without xs =
    case xs of
        [] ->
            []

        h :: t ->
            ( h, t ) :: List.map (Tuple.mapSecond ((::) h)) (without t)


withoutEach : List (List a) -> List (List ( a, List (List a) ))
withoutEach xs =
    case xs of
        [] ->
            []

        h :: t ->
            List.map (Tuple.mapSecond (\wa -> wa :: t)) (without h) :: List.map (List.map (Tuple.mapSecond ((::) h))) (withoutEach t)


off : Int -> Int -> Float
off i n =
    toFloat i - toFloat (n - 1) / 2


table : Container
table =
    { s = \i n -> { x = Card.spacing * off i n, y = 0 }
    , i = \pos n -> pos.x / Card.spacing + toFloat n / 2 |> floor |> clamp 0 n
    }


apply : Model -> Container -> List (CardData card) -> List (CardData card)
apply model container a =
    a


type alias Model =
    { auth : Discord.Auth
    , width : Int
    , height : Int
    , hands :
        { s : List (CardData Card)
        , w : List (CardData ())
        , n : List (CardData (Maybe Card))
        , e : List (CardData ())
        }
    , table : List (CardData Card)
    , selected : List (CardData Card)
    , mouse : Vector
    , selectionPos : Vector
    }


type alias CardData card =
    { pos : Tween, z : Int, card : card }



---- Util ----


scale : Model -> Float -> Float
scale model k =
    k * toFloat model.height


type Region
    = Table
    | HandSelf
    | HandTeammate
    | HandLeftPlayer
    | HandRightPlayer


region : Model -> Region
region model =
    if model.mouse.y < -0.25 then
        HandTeammate

    else if model.mouse.y < 0.1 then
        if model.mouse.x < -0.3 then
            HandLeftPlayer

        else if model.mouse.x < 0.3 then
            Table

        else
            HandRightPlayer

    else
        HandSelf



---- Init ----


init : Discord.Auth -> Int -> Int -> Model
init auth width height =
    (jump << recalculateContainers)
        { auth = auth
        , width = width
        , height = height
        , hands =
            { s =
                List.map initCardData
                    [ Card.R Card.Two Card.Spades
                    , Card.J Card.Red
                    , Card.R Card.Five Card.Clubs
                    , Card.R Card.Ace Card.Diamonds
                    , Card.R Card.Three Card.Spades
                    , Card.R Card.Jack Card.Hearts
                    , Card.R Card.Ace Card.Clubs
                    , Card.R Card.Three Card.Diamonds
                    , Card.R Card.Four Card.Clubs
                    , Card.R Card.Two Card.Hearts
                    , Card.R Card.Five Card.Spades
                    , Card.R Card.Three Card.Clubs
                    , Card.R Card.Seven Card.Clubs
                    , Card.R Card.Ten Card.Diamonds
                    , Card.R Card.Ten Card.Hearts
                    , Card.R Card.Five Card.Spades
                    , Card.R Card.Eight Card.Hearts
                    , Card.R Card.Queen Card.Hearts
                    , Card.R Card.Four Card.Hearts
                    , Card.R Card.Three Card.Clubs
                    , Card.R Card.Queen Card.Hearts
                    , Card.R Card.Queen Card.Diamonds
                    , Card.J Card.Black
                    , Card.R Card.Nine Card.Hearts
                    , Card.R Card.Ace Card.Spades
                    , Card.R Card.King Card.Spades
                    , Card.R Card.Four Card.Hearts
                    ]
            , w = List.repeat 27 (initCardData ())
            , n = List.repeat 27 (initCardData Nothing)
            , e = List.repeat 27 (initCardData ())
            }
        , table =
            List.map initCardData
                [ Card.R Card.Ace Card.Spades
                , Card.R Card.Four Card.Hearts
                , Card.R Card.Seven Card.Clubs
                , Card.R Card.Ten Card.Diamonds
                , Card.J Card.Black
                , Card.J Card.Red
                ]
        , selected = []
        , mouse = { x = 0, y = 0 }
        , selectionPos = { x = 0, y = 0 }
        }


initCardData : card -> CardData card
initCardData card =
    { z = 0, pos = Tween.init { x = 0, y = 0 }, card = card }



---- Update ----


type Msg
    = Tick Time.Posix
    | WindowResized Int Int
    | MouseDownHand (CardData Card) (List (CardData Card))
    | MouseDownTable (CardData Card) (List (CardData Card))
    | Mouse MouseAction ( Float, Float )
    | SortHand


type MouseAction
    = Move
    | Up


jump : Model -> Model
jump model =
    let
        jumpContainer : List (CardData card) -> List (CardData card)
        jumpContainer =
            List.map (\cd -> { cd | pos = Tween.init cd.pos.target })
    in
    { model
        | hands =
            { s = jumpContainer model.hands.s
            , w = jumpContainer model.hands.w
            , n = jumpContainer model.hands.n
            , e = jumpContainer model.hands.e
            }
        , table = jumpContainer model.table
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SortHand ->
            let
                hands =
                    model.hands
            in
            ( { model
                | hands =
                    { hands | s = model.hands.s |> List.sortWith (\cd1 cd2 -> Card.compare cd1.card cd2.card) }
              }
                |> recalculateContainers
            , Cmd.none
            )

        Tick _ ->
            let
                tickContainer : List (CardData card) -> List (CardData card)
                tickContainer =
                    List.map (\cd -> { cd | pos = Tween.tick cd.pos })
            in
            ( { model
                | hands =
                    { s = tickContainer model.hands.s
                    , w = tickContainer model.hands.w
                    , n = tickContainer model.hands.n
                    , e = tickContainer model.hands.e
                    }
                , table = tickContainer model.table

                -- , selected = List.map (\cd -> { cd | pos = Tween.init cd.pos.target }) model.selected
              }
            , Cmd.none
            )

        WindowResized w h ->
            ( { model | width = w, height = h } |> recalculateContainers |> jump, Cmd.none )

        MouseDownHand cd cds ->
            let
                hands =
                    model.hands
            in
            ( { model
                | hands = { hands | s = cds }
                , selectionPos = cd.pos.current
                , selected = [ { cd | pos = Tween.init cd.pos.current } ]
              }
                |> recalculateContainers
            , Cmd.none
            )

        MouseDownTable cd cds ->
            ( { model
                | table = cds
                , selectionPos = cd.pos.current
                , selected = [ { cd | pos = Tween.init cd.pos.current } ]
              }
                |> recalculateContainers
            , Cmd.none
            )

        Mouse action ( x2, y2 ) ->
            let
                mouse2 =
                    { x = (x2 - toFloat model.width / 2) / toFloat model.height, y = y2 / toFloat model.height - 1 / 2 }

                pos2 =
                    Vector.add model.selectionPos (Vector.sub mouse2 model.mouse)

                model2 =
                    { model | mouse = mouse2, selectionPos = pos2 }
            in
            (case action of
                Move ->
                    model2

                Up ->
                    case region model2 of
                        HandSelf ->
                            let
                                iInsertIntoHand =
                                    iInsert model (List.length model.hands.s)

                                hands =
                                    model.hands

                                handS =
                                    List.take iInsertIntoHand model.hands.s ++ model.selected ++ List.drop iInsertIntoHand model.hands.s
                            in
                            { model | hands = { hands | s = handS }, selected = [] }

                        Table ->
                            let
                                iInsertIntoTable =
                                    iInsert model (List.length model.table)

                                table2 =
                                    List.take iInsertIntoTable model.table ++ model.selected ++ List.drop iInsertIntoTable model.table
                            in
                            { model | table = table2, selected = [] }

                        _ ->
                            model
            )
                |> recalculateContainers
                |> (\model3 -> ( model3, Cmd.none ))


recalculateContainers : Model -> Model
recalculateContainers model =
    let
        withGap : Int -> Int -> List (CardData card) -> (Float -> Vector) -> List (CardData card)
        withGap iInsertion nInsertion container posFromOffset =
            List.indexedMap
                (\i cd ->
                    let
                        iAdjusted =
                            if i < iInsertion then
                                i

                            else
                                i + nInsertion

                        offset =
                            toFloat iAdjusted - toFloat (List.length container + nInsertion - 1) / 2
                    in
                    { pos = Tween.set (posFromOffset offset) cd.pos
                    , z = iAdjusted
                    , card = cd.card
                    }
                )
                container

        normal =
            withGap 0 0

        handW =
            normal model.hands.w (\offset -> { x = -0.35, y = -offset * Card.spacing })

        handN =
            normal model.hands.n (\offset -> { x = offset * Card.spacing, y = -0.35 })

        handE =
            normal model.hands.e (\offset -> { x = 0.35, y = offset * Card.spacing })
    in
    case region model of
        HandSelf ->
            let
                iInsertIntoHand =
                    iInsert model (List.length model.hands.s)

                handS =
                    withGap iInsertIntoHand
                        (List.length model.selected)
                        model.hands.s
                        (\offset -> { x = offset * Card.spacing, y = 0.35 })

                table2 =
                    normal model.table (\offset -> { x = offset * Card.spacing, y = 0 })

                selected2 =
                    normal model.selected (\offset -> { x = model.selectionPos.x + offset * Card.spacing, y = model.selectionPos.y })
                        |> List.map (\cd -> { cd | pos = Tween.init cd.pos.target, z = cd.z + iInsertIntoHand })
            in
            { model
                | hands = { s = handS, w = handW, n = handN, e = handE }
                , table = table2
                , selected = selected2
            }

        Table ->
            let
                iInsertIntoTable =
                    iInsert model (List.length model.table)

                handS =
                    normal model.hands.s (\offset -> { x = offset * Card.spacing, y = 0.35 })

                table2 =
                    withGap iInsertIntoTable
                        (List.length model.selected)
                        model.table
                        (\offset -> { x = offset * Card.spacing, y = 0 })

                selected2 =
                    normal model.selected (\offset -> { x = model.selectionPos.x + offset * Card.spacing, y = model.selectionPos.y })
                        |> List.map (\cd -> { cd | pos = Tween.init cd.pos.target, z = cd.z + iInsertIntoTable })
            in
            { model
                | hands = { s = handS, w = handW, n = handN, e = handE }
                , table = table2
                , selected = selected2
            }

        _ ->
            let
                handS =
                    normal model.hands.s (\offset -> { x = offset * Card.spacing, y = 0.35 })

                table2 =
                    normal model.table (\offset -> { x = offset * Card.spacing, y = 0 })

                selected2 =
                    normal model.selected (\offset -> { x = model.selectionPos.x + offset * Card.spacing, y = model.selectionPos.y })
                        |> List.map (\cd -> { cd | pos = Tween.init cd.pos.target })
            in
            { model
                | hands = { s = handS, w = handW, n = handN, e = handE }
                , table = table2
                , selected = selected2
            }


iInsert : Model -> Int -> Int
iInsert model n =
    (model.selectionPos.x
        / Card.spacing
        + toFloat (n + List.length model.selected)
        / 2
    )
        |> floor
        |> clamp 0 n



---- View ----


view : Model -> Html Msg
view model =
    Svg.svg
        [ HtmlA.width model.width
        , HtmlA.height model.height
        , HtmlE.onMove (.offsetPos >> Mouse Move)
        , HtmlE.onUp (.offsetPos >> Mouse Up)
        , HtmlE.onLeave (.offsetPos >> Mouse Up)
        ]
        [ Svg.defs []
            [ Svg.filter
                [ SvgA.id "cardShadow"
                , SvgA.x "-50%"
                , SvgA.width "150%"
                ]
                [ Svg.node "feDropShadow"
                    [ SvgA.dx (String.fromFloat <| scale model 0.001)
                    , SvgA.dy "0"
                    , SvgA.stdDeviation (String.fromFloat <| scale model 0.004)
                    , SvgA.floodColor "rgba(0,0,0,0.1)"
                    , SvgA.filterUnits "userSpaceOnUse"
                    ]
                    []
                ]
            ]
        , Transform.translate (Vector.mul 0.5 { x = toFloat model.width, y = toFloat model.height }) <|
            Svg.g []
                [ Svg.text_ [ SvgA.x "0", SvgA.y "0" ]
                    [ Svg.text (region model |> Debug.toString) ]
                , Svg.rect
                    [ SvgA.fill "#1a528a"
                    , SvgA.x (String.fromFloat <| toFloat model.width / 2 - scale model 0.25)
                    , SvgA.y (String.fromFloat <| scale model 0.92)
                    , SvgA.width (String.fromFloat <| scale model 0.5)
                    , SvgA.height (String.fromFloat <| scale model 0.06)
                    , SvgA.rx (String.fromFloat <| scale model 0.02)
                    , SvgA.cursor "pointer"
                    ]
                    []
                , Svg.text_ [ SvgA.x "-500", SvgA.y "-500" ]
                    [ Svg.text (region model |> Debug.toString) ]
                , Svg.rect
                    [ SvgA.fill "#a1662e"
                    , SvgA.x (String.fromFloat <| scale model -0.2)
                    , SvgA.y (String.fromFloat <| scale model -0.08)
                    , SvgA.width (String.fromFloat <| scale model 0.4)
                    , SvgA.height (String.fromFloat <| scale model 0.16)
                    , SvgA.rx (String.fromFloat <| scale model 0.01)
                    , SvgA.cursor "pointer"
                    ]
                    []
                , Svg.g [ SvgE.onClick SortHand ]
                    (model.hands.n
                        |> List.sortBy .z
                        |> List.map
                            (\cd ->
                                Card.faceDown (scale model 0.1)
                                    |> Transform.translate (Vector.mul (toFloat model.height) cd.pos.current)
                            )
                    )
                , Svg.g [ SvgE.onClick SortHand ]
                    (model.hands.w
                        |> List.sortBy .z
                        |> List.map
                            (\cd ->
                                Card.faceDown (scale model 0.1)
                                    |> Transform.rotate (turns 0.75)
                                    |> Transform.translate (Vector.mul (toFloat model.height) cd.pos.current)
                            )
                    )
                , Svg.g [ SvgE.onClick SortHand ]
                    (model.hands.e
                        |> List.sortBy .z
                        |> List.map
                            (\cd ->
                                Card.faceDown (scale model 0.1)
                                    |> Transform.rotate (turns 0.25)
                                    |> Transform.translate (Vector.mul (toFloat model.height) cd.pos.current)
                            )
                    )
                , Svg.g []
                    (let
                        mouseDownHand =
                            model.hands.s |> without |> List.map (\( cd, cds ) -> ( cd, [ SvgE.onMouseDown (MouseDownHand cd cds) ] ))

                        mouseDownTable =
                            model.table |> without |> List.map (\( cd, cds ) -> ( cd, [ SvgE.onMouseDown (MouseDownTable cd cds) ] ))

                        noAttributeSelected =
                            List.map (\cd -> ( cd, [] )) model.selected
                     in
                     (mouseDownHand ++ mouseDownTable ++ noAttributeSelected)
                        |> List.sortBy (Tuple.first >> .z)
                        |> List.map
                            (\( cd, attributes ) ->
                                Svg.g attributes [ Card.faceUp (scale model 0.12) cd.card ]
                                    |> Transform.translate (Vector.mul (toFloat model.height) cd.pos.current)
                            )
                    )

                -- , Svg.g []
                --     (let
                --         handCurved =
                --             arc { x = 0, y = scale model 1.5 } (scale model 1.25) 0 0.024
                --      in
                --      List.indexedMap
                --         (\i () ->
                --             Card.faceUp (scale model 0.14) (R Ace Spades)
                --                 |> Transform.rotate (handCurved.theta <| toFloat i - 7)
                --                 |> Transform.translate (handCurved.s <| toFloat i - 7)
                --         )
                --         (List.repeat 15 ())
                --     )
                -- , Svg.g []
                --     (let
                --         handCurved =
                --             arc { x = 0, y = scale model 1.5 } (scale model 1.2) 0 0.024
                --      in
                --      List.indexedMap
                --         (\i () ->
                --             Card.faceUp (scale model 0.14) (R Ace Spades)
                --                 |> Transform.rotate (handCurved.theta <| toFloat i - 13)
                --                 |> Transform.translate (handCurved.s <| toFloat i - 13)
                --         )
                --         (List.repeat 27 ())
                --     )
                ]
        ]
