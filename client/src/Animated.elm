module Animated exposing (..)

import Vector exposing (Vector)


type alias Animated =
    { current : Vector
    , target : Vector
    }


init : Vector -> Animated
init pos =
    Animated pos pos


set : Vector -> Animated -> Animated
set pos anim =
    { anim | target = pos }


tick : Animated -> Animated
tick { current, target } =
    let
        difference =
            Vector.sub target current
    in
    { current = Vector.add current (Vector.mul 0.2 difference)
    , target = target
    }
