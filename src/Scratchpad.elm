module Scratchpad exposing (..)

import Animator
import Animator.Inline

import  Color

import Dict exposing (Dict)

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
    , fromRgb
    )
import Element.Border as Border
import Element.Background as Background
import Element.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Element.Font as Font
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


type alias Model = 
    { menuOpen : Bool
    , checked : Animator.Timeline Bool
    , buttonStates : Animator.Timeline (Dict Id Bool)
    }


type alias Id = String


init : () -> (Model, Cmd Msg)
init _ =
    ( { menuOpen = False
      , checked = Animator.init False
      , buttonStates = Animator.init <|
            Dict.fromList [ ("Uno", False), ("Dos", False), ("Tres", False) ]
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        maybeAlways val =
            Maybe.map (\_ -> val)

        setButtonState id newState =
            Dict.update id (maybeAlways newState) <| Animator.current model.buttonStates

    in
    case msg of
        MenuOpen ->
            ( { model | menuOpen = True }
            , Cmd.none
            )
        MenuClose ->
            ( { model | menuOpen = False }
            , Cmd.none
            )
        Download ->
            ( model
            , Download.string "foo" "text/csv" "foo"
            )
        SetChecked bool ->
            ( { model
                | checked = Animator.go Animator.quickly bool model.checked
              }
            , Cmd.none
            )
        AnimationStep newTime ->
            ( Animator.update newTime animator model
            , Cmd.none
            )
        ElementHover id ->
            ( { model
                | buttonStates =
                    Animator.go Animator.slowly (setButtonState id True) model.buttonStates
              }
            , Cmd.none
            )
        ElementUnhover id ->
            ( { model
                | buttonStates =
                    Animator.go Animator.slowly (setButtonState id False) model.buttonStates
              }
            , Cmd.none
            )
            


type Msg
    = MenuOpen
    | MenuClose
    | Download
    | SetChecked Bool
    | ElementHover Id
    | ElementUnhover Id
    | AnimationStep Posix


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Animator.toSubscription AnimationStep model animator



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
                [ E.text (if model.menuOpen then "menu open" else "menu closed")
                , I.button
                    []
                    { label = E.text "download foo"
                    , onPress = Just Download
                    }
                , I.checkbox
                    [ E.htmlAttribute <| Animator.Inline.opacity model.checked <|
                        \checked ->
                            if checked then
                                Animator.at 1

                            else
                                Animator.at 0
                    ]
                    { onChange = SetChecked
                    , icon = I.defaultCheckbox
                    , checked = Animator.current model.checked
                    , label = I.labelLeft [] <| E.text "I am an animated checkbox!"
                    }
                , viewButtons model
                ]
            , column
                [ E.alignRight
                , E.alignTop
                ]
                [ menuButton [] MenuOpen
                ]
            ]


viewButtons : Model -> Element Msg
viewButtons model =
    let
        buttonState id =
            Maybe.withDefault False <| Dict.get id <| Animator.current model.buttonStates

        borderColor id =
            fromRgb <| Color.toRgba <|
                if buttonState id then
                    Color.blue

                else
                    Color.black

        fontColor id =
            fromRgb <| Color.toRgba <|
                if buttonState id then
                    Color.white

                else
                    Color.black

        bgColor id =
            fromRgb <| Color.toRgba <|
                Animator.color model.buttonStates <|
                    \buttonStates ->
                        if Maybe.withDefault False <| Dict.get id buttonStates then
                            Color.lightBlue

                        else
                            Color.white

        fontSize id = 20
           -- round <| Animator.linear model.buttonStates <|
           --     \buttonStates ->
           --         Animator.at <|
           --             if Maybe.withDefault False <| Dict.get id buttonStates then
           --                 28
           --             else
           --                 20

        button id =
            el
                [ E.width <| px 200
                , E.height <| px 60
                , Border.width 3
                , Border.rounded 6
                , Border.color <| borderColor id
                , Background.color <| bgColor id
                , Font.color <| fontColor id
                , Font.size <| fontSize id
                , padding 10
                , onMouseEnter <| ElementHover id
                , onMouseLeave <| ElementUnhover id
                ]
            <|
                (el [ centerX, centerY ] <| E.text <| "Button " ++ id)
    in
    [ "Uno", "Dos", "Tres" ]
        |> List.map button
        |> column [E.spacing 10, centerX, centerY ]


viewMenu : Model -> Element Msg
viewMenu model =
    if model.menuOpen then
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




-- ANIMATOR


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watchingWith
            .buttonStates
            (\newButtonStates model ->
                { model | buttonStates = newButtonStates }
            )
            (\buttonStates ->
                List.any identity <| Dict.values buttonStates
            )



-- COLOR


lightgrey = rgb 0.8 0.8 0.8
