module TextElements exposing (..)

import Element exposing (..)
import String
    exposing
        ( fromInt
        , fromChar
        )
import Char
    exposing
        ( fromCode
        )



-- LISTS


type TextList
    = Ol (List ListItem)
    | Ul (List ListItem)

type ListItem
    = Item String
    | SubList String TextList



indentSize = 30

ol : List (Attribute msg) -> List ListItem -> List (Element msg)
ol attrs textList =
    let
        preFns =
            [ (\idx -> (fromInt idx) ++ ". ")
            , (\idx -> (fromChar <| fromCode <| idx + 96) ++ ". ")
            ]
    in
        ol_ attrs { prefixFns = preFns, textList = textList, indent = indentSize }

ol_ : List (Attribute msg)
        -> { prefixFns : List (Int -> String)
            , textList : List ListItem
            , indent : Int
            }
        -> List (Element msg)
--ol : List (Attribute msg) -> (Int -> String) -> List String -> List (Element msg)
ol_ attrs ({indent, prefixFns, textList} as input) =
    let
        mkItem (idx, item) =
            let
                preFn =
                    case prefixFns of
                        [] -> String.fromInt
                        fn::fns -> fn
            in
            case item of
                Item str ->
                    [ paragraph
                        ( attrs ++
                            [ paddingEach 
                                { left = indent
                                , top = 0, right = 0, bottom = 0
                                }
                            ]
                        )
                        [ text <| (preFn idx) ++ str
                        ]
                    ]

                SubList str subList ->
                    (mkItem (idx, Item str))
                    ++ (case subList of
                            Ol listItems ->
                                ol_
                                    attrs
                                    { input
                                        | prefixFns = (Maybe.withDefault [] (List.tail prefixFns))
                                        , indent = indent + indentSize
                                        , textList = listItems
                                    }

                            Ul listItems ->
                                ul_
                                    attrs
                                    { input
                                        | prefixFns = (Maybe.withDefault [] (List.tail prefixFns))
                                        , indent = indent + indentSize
                                        , textList = listItems
                                    }
                        )
    in
        List.concatMap
                mkItem
                <| List.map2
                    (\idx item -> (idx, item))
                    (List.range 1 (List.length textList))
                    textList


ul : List (Attribute msg) -> List ListItem -> List (Element msg)
ul attrs textList = 
    let
        prefixFns =
            [ (\_ -> "\u{2022} ")
            , (\_ -> "\u{25e6} ")
            ]
    in
    ul_ attrs { prefixFns = prefixFns, indent = indentSize, textList = textList }


ul_ : List (Attribute msg) -> { prefixFns : List (Int -> String) , indent : Int , textList : List ListItem }
        -> List (Element msg)
ul_ attrs input = ol_ attrs input
