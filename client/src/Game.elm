module Game exposing (Action(..), Hands, State(..), Store(..), Team(..), decodePosition, decodeTeam)

import Card exposing (Card, Rank(..))
import Json.Decode as D


type State
    = Playing
        { hands : Hands
        , level : { ac : Rank, bd : Rank, who : Team }
        }
    | Trading
        { hands : Hands
        , level : { ac : Rank, bd : Rank, who : Team }
        , positions : Store Position
        , remaining : Store Int
        }


type alias Hands =
    { s : List Card
    , w : Int
    , n : Int
    , e : Int
    }


type Finished
    = NoOne
    | OnlyBigMaster Player
    | BothMasters Player Player


type Player
    = A
    | B
    | C
    | D


type Store a
    = Store a a a a


type Team
    = AC
    | BD


type Action
    = Revolt
    | Trade Card
    | Play (List Card)
    | Pass


type Position
    = BigMaster
    | SmallMaster
    | SmallSlave
    | BigSlave


decodeTeam : D.Decoder Team
decodeTeam =
    D.index 0 <|
        D.andThen
            (\variant ->
                case variant of
                    "AC" ->
                        D.succeed AC

                    "BD" ->
                        D.succeed BD

                    _ ->
                        D.fail "Unknown team variant"
            )
            D.string


decodePosition : D.Decoder Position
decodePosition =
    D.string
        |> D.andThen
            (\variant ->
                case variant of
                    "Big_master" ->
                        D.succeed BigMaster

                    "Small_master" ->
                        D.succeed SmallMaster

                    "Small_slave" ->
                        D.succeed SmallSlave

                    "Big_slave" ->
                        D.succeed BigSlave

                    _ ->
                        D.fail "Unknown position variant"
            )
