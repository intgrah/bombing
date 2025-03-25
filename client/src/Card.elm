module Card exposing (Card(..), Joker(..), Rank(..), Suit(..), compare, decode, decodeRank, encode, faceDown, faceUp, spacing)

import Json.Decode as D
import Json.Encode as E
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Transform



---- Model ----
{-
   A card is either a Black or Red Joker,
   or a card with Rank and Suit.
-}


type Card
    = R Rank Suit
    | J Joker


type Rank
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace


type Suit
    = Diamonds
    | Clubs
    | Hearts
    | Spades


type Joker
    = Black
    | Red


compare : Card -> Card -> Order
compare c1 c2 =
    case ( c1, c2 ) of
        ( R r1 _, R r2 _ ) ->
            Basics.compare (rankToInt r1) (rankToInt r2)

        ( J _, R _ _ ) ->
            GT

        ( R _ _, J _ ) ->
            LT

        ( J Black, J Red ) ->
            LT

        ( J Red, J Black ) ->
            GT

        _ ->
            EQ



---- View ----


faceUp : Float -> Card -> Svg msg
faceUp size card =
    let
        width =
            0.75 * size

        height =
            size
    in
    Svg.g []
        [ Svg.rect
            [ SvgA.fill "#f7f7f7"
            , SvgA.width (String.fromFloat <| width)
            , SvgA.height (String.fromFloat <| height)
            , SvgA.rx (String.fromFloat <| 0.0625 * height)
            , SvgA.cursor "pointer"
            , SvgA.filter "url(#cardShadow)"
            ]
            []
            |> Transform.translate { x = width / -2, y = height / -2 }
        , let
            pips =
                case card of
                    R rank suit ->
                        Svg.g []
                            [ Svg.text_
                                [ SvgA.x (String.fromFloat <| 0.12 * width)
                                , SvgA.y (String.fromFloat <| 0.15 * height)
                                , SvgA.fontSize (String.fromFloat <| 0.13 * height)
                                ]
                                [ Svg.text (rankToString rank) ]
                            , Svg.text_
                                [ SvgA.x (String.fromFloat <| 0.12 * width)
                                , SvgA.y (String.fromFloat <| 0.275 * height)
                                , SvgA.fontSize (String.fromFloat <| 0.13 * height)
                                ]
                                [ Svg.text (suitToString suit) ]
                            ]

                    J _ ->
                        Svg.text_
                            [ SvgA.x (String.fromFloat <| 0.1 * height)
                            , SvgA.y (String.fromFloat <| 0.15 * height)
                            , SvgA.fontSize (String.fromFloat <| 0.13 * height)
                            ]
                            [ Svg.text "❂" ]
          in
          Svg.g
            [ SvgA.fontFamily "sans"
            , SvgA.fontSize (String.fromFloat <| 0.2 * height)
            , SvgA.fontStyle "bold"
            , SvgA.fill (colourOf card)
            , SvgA.textAnchor "middle"
            , SvgA.style "user-select: none"
            , SvgA.cursor "pointer"
            ]
            [ pips |> Transform.translate { x = width / -2, y = height / -2 }
            , pips |> Transform.translate { x = width / -2, y = height / -2 } |> Transform.rotate (turns 0.5)
            ]
        ]


faceDown : Float -> Svg msg
faceDown size =
    let
        width =
            0.75 * size

        height =
            size
    in
    Svg.g []
        [ Svg.rect
            [ SvgA.fill "#3d5c8a"
            , SvgA.width (String.fromFloat <| width)
            , SvgA.height (String.fromFloat <| height)
            , SvgA.rx (String.fromFloat <| 0.0625 * height)
            , SvgA.cursor "pointer"
            , SvgA.filter "url(#cardShadow)"
            ]
            []
        , Svg.image
            [ SvgA.xlinkHref "/optiver-logo.svg"
            , SvgA.width (String.fromFloat <| 0.5 * width)
            , SvgA.height (String.fromFloat <| 0.2 * width)
            , SvgA.x (String.fromFloat <| 0.25 * width)
            , SvgA.y (String.fromFloat <| 0.3 * height)
            ]
            []
        , Svg.image
            [ SvgA.xlinkHref "/optiver-circle.png"
            , SvgA.width (String.fromFloat <| 0.4 * width)
            , SvgA.x (String.fromFloat <| 0.3 * width)
            , SvgA.y (String.fromFloat <| 0.5 * height)
            ]
            []
        ]
        |> Transform.translate { x = width / -2, y = height / -2 }


spacing : Float
spacing =
    0.18 * 0.1



---- Utils ----


colourOf : Card -> String
colourOf card =
    case card of
        J Red ->
            "#e05050"

        R _ Hearts ->
            "#e05050"

        R _ Diamonds ->
            "#e05050"

        J Black ->
            "#404040"

        R _ Spades ->
            "#404040"

        R _ Clubs ->
            "#404040"


rankToString : Rank -> String
rankToString rank =
    case rank of
        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"

        Ten ->
            "10"

        Jack ->
            "J"

        Queen ->
            "Q"

        King ->
            "K"

        Ace ->
            "A"


suitToString : Suit -> String
suitToString suit =
    case suit of
        Diamonds ->
            "♦"

        Clubs ->
            "♣"

        Hearts ->
            "♥"

        Spades ->
            "♠"


rankToInt : Rank -> Int
rankToInt rank =
    case rank of
        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Jack ->
            11

        Queen ->
            12

        King ->
            13

        Ace ->
            14



---- JSON encoders ----


encode : Card -> E.Value
encode card =
    case card of
        R r s ->
            E.list identity [ E.string "R", encodeRank r, encodeSuit s ]

        J j ->
            E.list identity [ E.string "J", encodeJoker j ]


encodeRank : Rank -> E.Value
encodeRank rank =
    case rank of
        Two ->
            E.string "Two"

        Three ->
            E.string "Three"

        Four ->
            E.string "Four"

        Five ->
            E.string "Five"

        Six ->
            E.string "Six"

        Seven ->
            E.string "Seven"

        Eight ->
            E.string "Eight"

        Nine ->
            E.string "Nine"

        Ten ->
            E.string "Ten"

        Jack ->
            E.string "Jack"

        Queen ->
            E.string "Queen"

        King ->
            E.string "King"

        Ace ->
            E.string "Ace"


encodeSuit : Suit -> E.Value
encodeSuit suit =
    case suit of
        Diamonds ->
            E.string "Diamonds"

        Clubs ->
            E.string "Clubs"

        Hearts ->
            E.string "Hearts"

        Spades ->
            E.string "Spades"


encodeJoker : Joker -> E.Value
encodeJoker joker =
    case joker of
        Black ->
            E.string "Black"

        Red ->
            E.string "Red"



---- JSON decoders ----


decode : D.Decoder Card
decode =
    D.index 0 D.string
        |> D.andThen
            (\variant ->
                case variant of
                    "R" ->
                        D.map2 R (D.index 1 decodeRank) (D.index 2 decodeSuit)

                    "J" ->
                        D.map J (D.index 1 decodeJoker)

                    -- D.map decodeJoker <| D.index 1 decodeJoker
                    _ ->
                        D.fail "Not a valid card!"
            )


decodeRank : D.Decoder Rank
decodeRank =
    D.index 0 <|
        D.andThen
            (\variant ->
                case variant of
                    "Two" ->
                        D.succeed Two

                    "Three" ->
                        D.succeed Three

                    "Four" ->
                        D.succeed Four

                    "Five" ->
                        D.succeed Five

                    "Six" ->
                        D.succeed Six

                    "Seven" ->
                        D.succeed Seven

                    "Eight" ->
                        D.succeed Eight

                    "Nine" ->
                        D.succeed Nine

                    "Ten" ->
                        D.succeed Ten

                    "Jack" ->
                        D.succeed Jack

                    "Queen" ->
                        D.succeed Queen

                    "King" ->
                        D.succeed King

                    "Ace" ->
                        D.succeed Ace

                    _ ->
                        D.fail "Not a valid Rank!"
            )
            D.string


decodeSuit : D.Decoder Suit
decodeSuit =
    D.index 0 <|
        D.andThen
            (\variant ->
                case variant of
                    "Diamonds" ->
                        D.succeed Diamonds

                    "Clubs" ->
                        D.succeed Clubs

                    "Hearts" ->
                        D.succeed Hearts

                    "Spades" ->
                        D.succeed Spades

                    _ ->
                        D.fail "Not a valid Suit!"
            )
            D.string


decodeJoker : D.Decoder Joker
decodeJoker =
    D.index 0 <|
        D.andThen
            (\variant ->
                case variant of
                    "Black" ->
                        D.succeed Black

                    "Red" ->
                        D.succeed Red

                    _ ->
                        D.fail "Not a valid Joker type!"
            )
            D.string
