module Scratchpad exposing (..)


import Browser
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Element as E exposing
    ( layout
    , row
    , column
    , el
    , rgb
    , px
    , Element
    , centerX
    , centerY
    , alignRight
    , alignTop
    , inFront
    , none
    , padding
    )
import Element.Border as Border
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Input as I

import String exposing (fromInt, fromFloat)
import Random
import Time exposing (Posix, now)
import Task exposing (perform, andThen)
import Process

import File.Download as Download



-- MAIN


main = Browser.element 
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model = Bool
    -- { menu : String
    -- }


init : () -> (Model, Cmd Msg)
init _ =
    ( False
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MenuOpen ->
            ( True
            , Cmd.none
            )
        MenuClose ->
            ( False
            , Cmd.none
            )
        Download ->
            ( model
            , Download.string "foo" "text/csv" "foo"
            )
            


type Msg
    = MenuOpen
    | MenuClose
    | Download



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    layout
        [ inFront <| viewMenu model
        ]
        <| row
            [ E.width E.fill
            ]
            [ E.html <| svg
                [ width <| fromFloat 500.0
                , height <| fromFloat 500.0
                , viewBox <| "0 0 " ++ fromFloat 500.0 ++ " " ++ fromFloat 500.0
                , fill "rgb(51,51,51)"
                , enableBackground "1"
                -- , centerX
                ]
                [ rect
                    [ x "10"
                    , y "100"
                    , width "100"
                    , height "100"
                    , fill "rgb(51,51,51)"
                    ]
                    []
                , polygon
                    [ points <| pointsToString <| makeCrossPolygonPoints (150,150) 20
                    , stroke "rgb(102,102,102)"
                    , strokeWidth "5"
                    ]
                    []
                , text_
                    [ x "10"
                    , y "100"
                    , fontFamily "Lato light"
                    , fontSize "36"
                    --, fontStyle "italic"
                    --, fontWeight "bold"
                    ]
                    [ text "foo"
                    ]
                ]
            , column
                [centerX, centerY] 
                [ E.text (if model then "menu open" else "menu closed")
                , I.button
                    []
                    { label = E.text "download foo"
                    , onPress = Just Download
                    }
                ]
            , column
                [ E.alignRight
                , E.alignTop
                ]
                [ menuButton [] MenuOpen
                ]
            ]


viewMenu : Model -> Element Msg
viewMenu model =
    if model then
        menuPanel
            [ alignRight
            , alignTop
            ]
            [ E.text "foo"
            , E.text "bar"
            ]
    else
        none

menuButton : List (E.Attribute Msg) -> Msg -> Element Msg
menuButton attrs msg =
    let
        radius = 6
        dot : Int -> Element msg
        dot r = 
            el
                [ Border.rounded r
                , E.width <| px r
                , E.height <| px r
                , Background.color lightgrey
                ]
                none
    in
        column
            ( [ padding <| 2 * radius
              , E.spacing radius
              , onClick msg
              ]
              ++ attrs
            )
            <| List.repeat 3 (dot radius)


menuPanel : List (E.Attribute Msg) -> List (Element Msg) -> Element Msg
menuPanel attrs els =
    el
        [ E.width E.fill
        , E.height E.fill
        , Background.color <| E.rgba 0 0 0 0.5
        , onClick MenuClose
        , inFront
            <| column
                ( [ Border.rounded 6
                  , Background.color <| lightgrey
                  , E.padding 10
                  ]
                  ++ attrs
                )
                ([] ++ els)
        ]
        none


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


lightgrey = rgb 0.8 0.8 0.8
