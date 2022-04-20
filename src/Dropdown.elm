module Dropdown exposing (..)


import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Events exposing (onClick)

background = rgb 0.2 0.2 0.2
grey = rgb 0.6 0.6 0.6
lightgrey = rgb 0.8 0.8 0.8

type Dropdown a
    = Selected (Maybe a) (List a)
    | Selecting (Maybe a) (List a)


openDD : Dropdown a -> Dropdown a
openDD dd =
    case dd of
        Selected cur items -> Selecting cur items
        _ -> dd


closeDD : Maybe a -> Dropdown a -> Dropdown a
closeDD new dd =
    case dd of
        Selecting prev items ->
            case new of
                Just item ->
                    Selected new items

                Nothing ->
                    Selected prev items

        _ -> dd


type DDAction a
    = OpenList
    | Select a


dropdown : Dropdown a -> (a -> String) -> (DDAction a -> msg) -> Element msg
dropdown dd toString toMsg =
        let
            selectedName =
                case dd of
                    Selected (Just someA) _ -> toString someA
                    Selected Nothing _ -> "Click to select"
                    Selecting Nothing _ -> "Click to select"
                    Selecting (Just prev) _ -> toString prev

            attrs : List (Attribute msg)
            attrs =
                case dd of
                    Selecting prev items ->
                        let
                            mouseOverColor : Color
                            mouseOverColor = rgb 0.9 0.9 0.1
                                        
                            backgroundColor : Color
                            backgroundColor = rgb 1 1 1
                                    
                            viewItem : a -> Element msg
                            viewItem item =
                                el
                                    [ width fill
                                    , mouseOver
                                        [ Background.color grey ]
                                    , Background.color lightgrey
                                    , Font.color background
                                    , width <| px 130
                                    , onClick <| toMsg (Select item)
                                    ]
                                    (text <| toString item)
                            viewItemsList : List a -> Element msg
                            viewItemsList list =
                                column [] <|
                                    List.map viewItem list
                        in
                            [ below (viewItemsList items) ]

                    Selected item _ ->
                            [ onClick <| toMsg OpenList ]

        in
            el
                ( [ Border.width 1
                  , width <| px 130
                  , padding 3
                  ]
                  ++ attrs
                )
                <| text selectedName
