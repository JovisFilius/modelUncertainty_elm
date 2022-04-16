module Rocket exposing (..)

import Browser
import Html exposing (Html)

import Element exposing (layout, text)
import Svg exposing (circle, svg)
import Svg.Attributes exposing (width, height, cx, cy, r, viewBox)
import String exposing (fromInt)


main = Browser.sandbox { init = init, update = update, view = view }


-- MODEL


type alias Model =
    { x : Int
    , y : Int
    , r : Int
    }


init : Model
init =
    { x = 50
    , y = 50
    , r = 10
    }



-- UPDATE


type Msg = NoOp


update : Msg -> Model -> Model
update _ model = model



-- VIEW


view : Model -> Html Msg
view model =
    svg
        [ width "120"
        , height "120"
        , viewBox "0 0 120 120"
        ]
        [ circle
            [ cx <| fromInt model.x
            , cy <| fromInt model.y
            , r <| fromInt model.r
            ]
            []
        ]
        
    -- layout
    --     []
    --     <| text "rocket"
        
