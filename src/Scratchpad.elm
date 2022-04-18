module Scratchpad exposing (..)


import Browser
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


import String exposing (fromInt, fromFloat)
import Random
import Time exposing (Posix, now)
import Task exposing (perform, andThen)
import Process

main =
    svg
        [ width <| fromFloat 1000.0
        , height <| fromFloat 1000.0
        , viewBox <| "0 0 " ++ fromFloat 1000.0 ++ " " ++ fromFloat 1000.0
        , fill "rgb(51,51,51)"
        , enableBackground "1"
        -- , centerX
        ]
        [ rect
            [ x "10"
            , y "100"
            , width "100"
            , height "100"
            , fill "rgb(255,0,0)"
            ]
            []
        , polygon
            [ points <| pointsToString <| makeCrossPolygonPoints (150,150) 20
            , stroke "rgb(0,255,255)"
            , strokeWidth "5"
            ]
            []
        ]

makeCrossPolygonPoints : (Float, Float) -> Float -> List (Float,Float)
makeCrossPolygonPoints (x,y) w =
    [(x,y), (x+w/2, y+w/2), (x,y), (x+w/2, y-w/2), (x,y), (x-w/2, y-w/2), (x,y), (x-w/2,y+w/2)]


pointsToString : List (Float,Float) -> String
pointsToString points =
    case points of
        [] ->
            ""

        (x,y)::ps ->
            fromFloat x ++ "," ++ fromFloat y ++ " " ++ pointsToString ps
