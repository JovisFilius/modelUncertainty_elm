module Rocket exposing (..)

import Browser
import Html exposing (Html)

import Element exposing (layout, text)
import Svg exposing (Svg, svg, circle, ellipse, rect)
import Svg.Attributes exposing  ( width, height, cx, cy, r
                                , rx, ry, viewBox, color, fill
                                , enableBackground
                                )
import String exposing (fromInt, fromFloat)


main = Browser.sandbox { init = init, update = update, view = view }


-- MODEL


type alias Model =
    { x : Float
    , y : Float
    , r : Float
    }


init : Model
init =
    { x = 150
    , y = 150
    , r = 10
    }



-- UPDATE


type Msg = NoOp


update : Msg -> Model -> Model
update _ model = model



-- VIEW


view : Model -> Html Msg
view model =
    let
        screenWidth = 920
        screenHeight = 920
        initialSeg =
            { rx = model.r
            , ry = model.r
            , dy = 0.0
            , red = 255.0
            , green = 255.0
            , blue = 255.0
            }
    in
    svg
        [ width <| fromInt screenWidth
        , height <| fromInt screenHeight
        , viewBox <| "0 0 " ++ fromInt screenWidth ++ " " ++ fromInt screenHeight
        , fill "rgb(51,51,51)"
        , enableBackground "1"
        ]
        <| [ rect
                [ cx <| fromFloat (screenWidth / 2)
                , cy <| fromFloat (screenHeight / 2)
                , width <| fromInt screenWidth
                , height <| fromInt screenHeight
                ]
                []
            ]
        ++ List.reverse (List.map (viewSegment model.x model.y)
        ([initialSeg] ++ makeTailSegments 5 initialSeg))


type alias Segment =
    { rx : Float
    , ry : Float
    , dy : Float
    , red : Float
    , green : Float
    , blue : Float
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


viewSegment : Float -> Float -> Segment -> Svg Msg
viewSegment x y seg =
    ellipse
        [ cx <| fromFloat x
        , cy <| fromFloat (y + seg.dy)
        , rx <| fromFloat seg.rx
        , ry <| fromFloat seg.ry
        , fill <| segmentColor seg
        -- , fill <| fromFloat seg.red ++ " " ++ fromFloat seg.green ++ " " ++ fromFloat seg.blue
        ]
        []


segmentColor : Segment -> String
segmentColor seg =
    "rgb(" ++ fromFloat seg.red ++ "," ++ fromFloat seg.green ++ "," ++ fromFloat seg.blue ++ ")"
