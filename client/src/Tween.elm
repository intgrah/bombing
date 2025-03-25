module Tween exposing (..)

import Vector exposing (Vector)


type alias Tween =
    { current : Vector
    , target : Vector
    }


init : Vector -> Tween
init pos =
    Tween pos pos


set : Vector -> Tween -> Tween
set pos tween =
    { tween | target = pos }


tick : Tween -> Tween
tick { current, target } =
    let
        difference =
            Vector.sub target current
    in
    { current = Vector.add current (Vector.mul 0.2 difference)
    , target = target
    }
