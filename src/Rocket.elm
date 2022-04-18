module Rocket exposing (..)

import Browser
import Html exposing (Html)

import Element as E
import Svg as S exposing (Svg, svg, circle, ellipse, rect)
import Svg.Attributes as SA exposing  ( width, height, cx, cy, r
                                , rx, ry, viewBox, color, fill
                                , enableBackground
                                )
import String exposing (fromInt, fromFloat)


-- main = Browser.sandbox { init = init, update = update, view = testView }


-- MODEL


type alias Rocket =
    { x : Float
    , y : Float
    , r : Float
    }


init : Rocket
init =
    { x = 550
    , y = 750
    , r = 10
    }



-- UPDATE


-- type Msg = NoOp


-- update : Msg -> Rocket -> Rocket
-- update _ rocket = rocket



-- VIEW


-- testView : Rocket -> Html Msg
-- testView rocket =
--     let
--         screenWidth = 920
--         screenHeight = 920
--     in
--     E.layout
--         []
--         <| E.column
--             [ E.centerX
--             , E.centerY
--             , E.height E.fill
--             --, E.width E.fill
--             , E.padding 75
--             --, explain Debug.todo
--             ]
--             [ E.html <| svg
--                 [ width <| fromInt screenWidth
--                 , height <| fromInt screenHeight
--                 , viewBox <| "0 0 " ++ fromInt screenWidth ++ " " ++ fromInt screenHeight
--                 , fill "rgb(51,51,51)"
--                 , enableBackground "1"
--                 ]
--                 <| rect
--                         [ cx <| fromFloat (screenWidth / 2)
--                         , cy <| fromFloat (screenHeight / 2)
--                         , width <| fromInt screenWidth
--                         , height <| fromInt screenHeight
--                         ]
--                         []
--                 ::  view rocket
--             ]


view : Rocket -> List (Svg msg)
view rocket =
    let 
        tip = initialSeg rocket
    in
        List.reverse
            <| List.map (viewSegment rocket.x rocket.y)
                ([tip] ++ makeTailSegments 5 tip)


type alias Segment =
    { rx : Float
    , ry : Float
    , dy : Float
    , red : Float
    , green : Float
    , blue : Float
    }


initialSeg : Rocket -> Segment
initialSeg rocket =
    { rx = rocket.r
    , ry = rocket.r
    , dy = 0.0
    , red = 242.25
    , green = 242.25
    , blue = 242.25
    }


makeTailSegments : Int -> Segment -> List Segment
makeTailSegments n seg =
    if n <= 0 then
        []
    else
        let 
            xDecay = 0.6
            yDecay = 0.8
            cDecay = 0.8
            newSeg = 
                { rx = seg.rx * xDecay
                , ry = seg.ry * yDecay
                , dy = seg.dy + seg.ry * ( 1.0 + 0.2 * yDecay )
                , red = seg.red * cDecay
                , green = seg.green * cDecay
                , blue = seg.blue * cDecay
                }
        in
            [newSeg] ++ makeTailSegments (n-1) newSeg


viewSegment : Float -> Float -> Segment -> Svg msg
viewSegment x y seg =
    ellipse
        [ cx <| fromFloat x
        , cy <| fromFloat (y + seg.dy)
        , rx <| fromFloat seg.rx
        , ry <| fromFloat seg.ry
        , fill <| segmentColor seg
        ]
        []


segmentColor : Segment -> String
segmentColor seg =
    "rgb(" ++ fromFloat seg.red ++ "," ++ fromFloat seg.green ++ "," ++ fromFloat seg.blue ++ ")"


