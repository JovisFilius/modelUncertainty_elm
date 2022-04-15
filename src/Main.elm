module Main exposing (..)

import Browser
import Html exposing (Html)
--import Html.Events exposing (onClick)
import Element exposing (..)
import Element.Input exposing
    ( button
    , slider
    , defaultThumb
    , labelLeft
    , labelBelow
    )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font

import String exposing (fromInt)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { spacing : Int
    , padding : Int
    }


init : Model
init =
    { spacing = 24
    , padding = 24
    }



-- UPDATE


type Msg
    = SetSpacing Int
    | IncrSpacing
    | DecrSpacing
    | SetPadding Int
    | IncrPadding
    | DecrPadding


update : Msg -> Model -> Model
update msg model =
  case msg of
    SetSpacing x ->
        { model | spacing = x }

    IncrSpacing ->
        update (SetSpacing <| model.spacing + 3) model

    DecrSpacing ->
        update (SetSpacing <| model.spacing - 3) model

    SetPadding x ->
        { model | padding = x }

    IncrPadding ->
        update (SetPadding <| model.padding + 3) model

    DecrPadding ->
        update (SetPadding <| model.padding - 3) model



-- VIEW


view : Model -> Html Msg
view model =
    layout
        []
        <|
        column
            [ centerX
            , centerY
            , height fill
            , width fill
            , padding 75
            ]
            [ heightFiller
            , row
                [ spacing model.spacing
                , padding model.padding
                , width fill
                , Border.color grey
                , Border.rounded 2
                , Border.width 2
                ]
                [ rect 15
                , text "Welcome"
                , el
                    [ centerX
                    , Border.width 1
                    , Border.color grey
                    , Font.size 12
                    , padding 3
                    , width <| px 150
                    ]
                    <| text "Search "
                , el [ alignRight ] <| text "foo"
                , el [ alignRight ] <| text "bar"
                ]
            , row
                [ centerX
                , alignBottom
                , spacing 85
                , padding 85
                ]
                [ propertySlider "Padding" model.padding SetPadding
                , propertySlider "Spacing" model.spacing SetSpacing
                ]
            ]

rect : Int -> Element Msg
rect w = 
    el
        [ Background.color grey
        , Border.rounded 2
        , height fill
        , width <| px w
        ]
        none

dot : Element Msg
dot = rect 5

heightFiller =
    el
        [ height fill
        ]
        none


propertySlider : String -> Int -> (Int -> Msg) -> Element Msg
propertySlider prop val msgFn = 
    el
        [ width <| px 100
        , centerX
        ]
        <| slider
            [ height <| px 30
            , behindContent
                <| el
                    [ width fill
                    , height <| px 2
                    , centerY
                    , centerX
                    , Background.color grey
                    , Border.rounded 2
                    ]
                    none
            ]
            { onChange = msgFn << round
            , label = labelBelow []
                        <| text <| prop ++ ": " ++ fromInt val
            , min = 0
            , max = 100
            , value = toFloat val
            , thumb = defaultThumb
            , step = Just 1
            }

grey = rgb 0.6 0.6 0.6
