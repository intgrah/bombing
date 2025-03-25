module Transform exposing (rotate, scale, translate)

import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Vector exposing (Vector)


type alias Transformation msg =
    Svg msg -> Svg msg


translate : Vector -> Transformation msg
translate { x, y } node =
    Svg.g [ SvgA.transform ("translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")") ] [ node ]


scale : Float -> Transformation msg
scale k node =
    Svg.g [ SvgA.transform ("scale(" ++ String.fromFloat k ++ ")") ] [ node ]


rotate : Float -> Transformation msg
rotate radians node =
    let
        degrees =
            180 * radians / pi
    in
    Svg.g [ SvgA.transform ("rotate(" ++ String.fromFloat degrees ++ ", 0, 0)") ] [ node ]
