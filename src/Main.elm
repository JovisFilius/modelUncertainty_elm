module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyPress, onAnimationFrame, onResize)
import Browser.Dom exposing (Viewport, getViewport)
import Url exposing (Url)
import Html exposing (Html)
--import Html.Events exposing (onClick)
import Json.Decode as Decode

import Element exposing (..)
import Element.Input exposing
    ( button
    , slider
    , defaultThumb
    , labelLeft
    , labelBelow
    , checkbox
    , defaultCheckbox
    , labelRight
    )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Events exposing (onClick)
import Svg exposing (Svg, svg)
import Svg.Attributes as SA

import Rocket as R

import String exposing (fromInt, fromFloat)
import Random
import Time exposing (Posix, now)
import Task exposing (perform, andThen)
import Process



-- TODO


type Todo = P String (List Todo) | Do String | Doing String | Done String | Cancel String

todoList : List String -> List Todo
todoList = List.map Do

todo : List Todo
todo =
    [ P "side menu bar"
        [ Doing "overlay elements over other elements (Element.inFront, Element.behindContent)"
        , Do "Show menu dots in upper right corner"
        , Do "Show save button in side menu"
        , Do "Show success percentage in side menu"
        , Do "Show trialIdx in side menu"
        ]
    , P "Welcome screen"
            [ Do "Replace button with 'press space to start'"
            , Do "Make a demo mode to explain the experiment"
            ]
    , P "Experiment logistics"
            [ Do "Make sure the experiment has the correct no. trials"
            , P "Make sure the right participant has the correct generative model in each session"
                [ Do "Make an input/dropdown field for participants to have an id"
                , Do <| "Make a counter/dropdown to select the index of the"
                    ++ "session -> each session index corresponds to a generative"
                    ++ "model in a deterministic manner"
                ]
            , P "Give task feedback after each trial"
                [ Cancel "Only show target when it is hit"
                , P "keep both rocket and target visible after hit until the next trial initiates"
                    [ Done "Refactor StepRocket to take an absolute time instead of a delta"
                    , Done "Refactor the update StepRocket to Finish the trial internally if it reaches the target height"
                    ]
                , P "make indicator for trial failure"
                    [ Done "Show cross icon"
                    , Done "Write feedback about the error"
                    ]
                , Done "make indicator for trial success"
                ] 
            , P "Make the experiment screen the proper size"
                [ P "Make screen width a function of screen height"
                    [ Done "initialize screen height: getViewPort : Viewport.viewport.height : Float"
                    , Done "subscribe to screen size changes -> Browser.Events.onResize"
                    ]
                , Do "Constrain screen width such that it always fits on the page"
                ]
            ]
    , P "Debug Log"
        [ Done "Show new debug messages at the top -> append to head of debugLog"
        ]
    , P "Save data"
        [ Do "save string contens to file"
        ]
    ]



-- MAIN


main =
     Browser.element
         { init = init
         , update = update
         , subscriptions = subscriptions
         , view = view
         }


-- MODEL


type ProgramState
    = WelcomeState Bool (List String)
    | Running ExperimentState


type alias ExperimentState =
        { params : Params
        , height : Float
        , width : Float 
        , results : List Trial
        , currentTrial : Trial
        , distModel : DistModel
        , debugLog : List String
        , cue : Bool
        , target : Bool
        , debug : Bool
        , menu : Bool
        }


init : () -> (ProgramState, Cmd Msg)
init _ =
    ( WelcomeState False []
    , Cmd.none
    )

viewport2HeightChanged : Viewport -> Msg
viewport2HeightChanged v =
    let 
        width = round v.viewport.width
        height = round v.viewport.height
    in
        HeightChanged height


initExperiment : ProgramState -> Float -> ProgramState
initExperiment state x =
    case state of
        WelcomeState doDebug debugLog ->
            Running
                { params = defaultParams
                , height = 740
                , width = 960
                , results = []
                , currentTrial = newTrial x
                , distModel = Random.weighted (100, Constant) []
                , debugLog = debugLog
                , cue = False
                , target = False
                , debug = doDebug
                , menu = False
                }

        _ -> state


type alias Params =
    { trialDelayMin : Int
    , trialDelayMax : Int
    , cueDuration : Float
    , targetDuration : Float
    , iti : Float
    , nTrials : Int
    -- , stepY : Float
    , flightDuration : Float
    , stepT : Int
    , rocketSizeFrac : Float
    , targetSizeFrac : Float
    , cueWidthFrac : Float
    , feedbackSizeFrac : Float
    , startYFrac : Float
    , targetYFrac : Float
    , aspectRatio : Float
    }


defaultParams : Params
defaultParams =
    { trialDelayMin = 150
    , trialDelayMax = 1000
    , cueDuration = 35.0
    , targetDuration = 150.0
    , iti = 1500.0
    , nTrials = 80
    -- , stepY = 0.1
    , flightDuration = 300.0
    , stepT = 1
    , rocketSizeFrac = 0.0075
    , targetSizeFrac = 0.0075
    , feedbackSizeFrac = 0.075
    , cueWidthFrac = 0.0375
    , startYFrac = 0.25
    , targetYFrac = 0.75
    , aspectRatio = 8/6
    }


type alias Trial =
    { xFrac : Float
    , error : Float
    , result : Bool
    , profile : Profile
    , status : TrialStatus
    , rocketY : Float
    , initialDelay : Float
    , startTime : Posix
    , launchTime : Posix
    , arrivalTime : Posix
    }


type TrialStatus
    = Idle
    | Waiting
    | Launchable
    | Launching
    | Finished Bool


type Profile
    = Constant
    | Linear
    | Quadratic


trialIdx : ExperimentState -> Int
trialIdx state =
    state.results |> List.length |> (+) 1


statusToString : Trial -> String
statusToString trial =
    case trial.status of
        Idle -> "idle"
        Waiting -> "initialwait"
        Launchable -> "launchable"
        Launching -> "launching"
        Finished flying -> "finished [result="
                    ++ (if trial.result then "success" else "failure")
                    ++ " [err="++fromFloat trial.error ++"]"
                    ++ ",stillFlying="++(if flying then "True" else "False")++"]]"
        

type alias DistModel = Random.Generator Profile



-- UPDATE


type Msg
    = MakeTrial
    | SpacePressed
    | PrepTrial Float
    | StartTrial Posix
    | HideCue
    | ShowTarget
    | HideTarget
    | LaunchRocket Posix
    | StepRocket Posix
    | SampleX Float
    | HeightChanged Int
    | Debug String
    | SetDebugging Bool
    | MenuOpen
    | MenuClose
    | NoOp


update : Msg -> ProgramState -> ( ProgramState, Cmd Msg )
update msg programState =
    case programState of
        WelcomeState doDebug debugLog -> 
            case msg of
                MakeTrial ->
                    ( programState
                    , Random.generate SampleX (Random.float 0 1)
                    )

                SampleX x ->
                    ( initExperiment programState x
                    , Task.perform viewport2HeightChanged getViewport
                    )

                SetDebugging bool ->
                    ( WelcomeState bool debugLog
                    , Cmd.none
                    )

                Debug str ->
                    ( WelcomeState doDebug (str::debugLog)
                    , Cmd.none
                    )

                _ ->
                    (programState, Cmd.none)

        Running state ->
            (\(newState, cmd) -> (Running newState, cmd)) <| updateExperimentState msg state


updateExperimentState : Msg -> ExperimentState -> (ExperimentState, Cmd Msg)
updateExperimentState msg ({currentTrial} as state) =
    case msg of
        Debug str ->
            ( { state | debugLog = state.debugLog ++ [str] }
            , Cmd.none
            )

        MakeTrial ->
            ( { state
              | debugLog =
                    ("MakeTrial [trialIdx="++(fromInt <| trialIdx state)++"]")::state.debugLog
              }
            , Random.generate SampleX (Random.float 0 1)
            )

        SampleX x ->
            ( { state
              | currentTrial = newTrial x
              , debugLog = ("sampleX [x="++fromFloat x++"]")::state.debugLog
              }
            , Cmd.none
            )

        SpacePressed ->
            case currentTrial.status of
                Idle ->
                    ( { state | debugLog = "SpacePressed"::state.debugLog
                      }
                    , Random.generate PrepTrial (Random.float 150.0 1000.0)
                    )
                    -- , Task.perform PrepTrial Time.now

                Launchable ->
                    ( { state
                      | debugLog = "SpacePressed"::state.debugLog
                      }
                    , Task.perform LaunchRocket Time.now
                    )

                _ ->
                    ( state, Cmd.none )

        PrepTrial t ->
            ( { state
              | currentTrial = prepareTrial t state.currentTrial
              , debugLog = ("PrepTrial [delay=" ++ (fromFloat t) ++ "] ")::state.debugLog
              }
            , Task.perform StartTrial (Process.sleep t |> andThen (\_ -> now))
            )

        StartTrial time ->
            let
                durationStr = fromFloat <| intendedDuration state.currentTrial
            in
                ( { state
                  | currentTrial = startTrial time state.currentTrial
                  , cue = True
                  , debugLog = ("StartTrial [intendedDuration="++durationStr++"]")::state.debugLog
                  }
                , Task.perform (\_ -> HideCue) (Process.sleep state.params.cueDuration)
                )

        HideCue ->
            ( { state
              | cue = False
              , debugLog = "HideCue"::state.debugLog
              }
            , Task.perform
                (\_ -> ShowTarget)
                (Process.sleep <|
                    (intendedDuration state.currentTrial)
                    - state.params.cueDuration
                    - state.params.targetDuration / 2
                )
            )

        ShowTarget ->
            ( { state
              | target = True
              , debugLog = "ShowTarget"::state.debugLog
              }
            , Task.perform (\_ -> HideTarget) (Process.sleep state.params.targetDuration)
            )

        HideTarget ->
            ( { state
              | target = False
              , debugLog = "HideTarget"::state.debugLog
              }
            , Cmd.none
            )

        LaunchRocket time ->
            ( { state
              | currentTrial = launchTrial time state.currentTrial
              , debugLog = "LaunchRocket"::state.debugLog
              }
            , Cmd.none
            )

        StepRocket currentTime ->
            case currentTrial.status of
                Finished True ->
                    let
                        dt = toFloat
                            <| (Time.posixToMillis currentTime)
                            - (Time.posixToMillis currentTrial.launchTime)

                        dy = dt / state.params.flightDuration

                        updatedTrial =
                            { currentTrial
                            | rocketY = dy
                            , status = Finished (dy <= 2.0)
                            }
                    in
                        ( { state
                          | currentTrial = updatedTrial
                          , debugLog = ("StepRocket [position="++fromFloat dy++"]")::state.debugLog
                          }
                        , if updatedTrial.status == Finished False then
                            Task.perform (\_ -> MakeTrial) (Process.sleep state.params.iti)
                        else
                            Cmd.none
                        )

                Launching ->
                    let
                        dt = toFloat
                            <| (Time.posixToMillis currentTime)
                            - (Time.posixToMillis currentTrial.launchTime)

                        dy = dt / state.params.flightDuration

                        slack = state.params.targetDuration / 2

                        updatedTrial =
                            if dy >= 1.0 then
                                finishTrial currentTime slack currentTrial
                            else
                                { currentTrial
                                | rocketY = dy
                                }
                    in
                        case updatedTrial.status of
                            Launching ->
                                ( { state
                                  | currentTrial = updatedTrial
                                  , debugLog = ("StepRocket [position="++fromFloat dy++"]")::state.debugLog
                                  }
                                , Cmd.none
                                )

                            Finished keepFlying ->
                                ( { state
                                  | currentTrial = updatedTrial
                                  , results = state.results ++ [updatedTrial]
                                  , debugLog = ("finishTrial "++statusToString updatedTrial)::state.debugLog
                                  }
                                , if keepFlying then
                                    Cmd.none
                                else
                                    Task.perform (\_ -> MakeTrial) (Process.sleep state.params.iti)
                                )

                            _ -> (state, Cmd.none) -- Should never be reached

                _ -> (state, Cmd.none)

        HeightChanged h ->
            let
                newWidth = (toFloat h) * state.params.aspectRatio
                newHeight = toFloat h
            in
            ( { state
              | width = newWidth
              , height = newHeight
              , debugLog =  ("HeightChanged [h="++fromInt h++"]")::state.debugLog
              }
            , Cmd.none
            )

        SetDebugging bool ->
            ( { state | debug = bool }
            , Cmd.none
            )

        MenuOpen ->
            ( { state | menu = True }
            , Cmd.none
            )

        MenuClose ->
            ( { state | menu = False }
            , Cmd.none
            )

        NoOp ->
            ( state, Cmd.none )


newTrial : Float -> Trial
newTrial x =
    { startTime = Time.millisToPosix -1
    , launchTime = Time.millisToPosix -1
    , arrivalTime = Time.millisToPosix -1
    , initialDelay = -1
    , xFrac = x
    , profile = Constant
    , error = -1
    , result = False
    , status = Idle
    , rocketY = 0
    }


prepareTrial : Float -> Trial -> Trial
prepareTrial delay trial =
    case trial.status of
        Idle ->
            { trial
            | initialDelay = delay
            , status = Waiting
            }

        _ -> trial


startTrial : Posix -> Trial -> Trial
startTrial time trial =
    case trial.status of
        Waiting ->
            { trial
            | startTime = time
            , status = Launchable
            }

        _ ->
            trial


launchTrial : Posix -> Trial -> Trial
launchTrial time trial =
    case trial.status of
        Launchable ->
            { trial
            | launchTime = time
            , status = Launching
            }

        _ -> trial


finishTrial : Posix -> Float -> Trial -> Trial
finishTrial arrivalTime slack trial =
    case trial.status of
        Launching ->
            let
                duration = Time.posixToMillis arrivalTime - Time.posixToMillis trial.startTime
                error = toFloat duration - (intendedDuration trial)
                result = (abs error) <= slack
            in
                { trial
                | arrivalTime = arrivalTime
                , status = Finished (not result)
                , error = error
                , result = result
                , rocketY =
                    if result then
                        1.0
                    else
                        trial.rocketY
                }

        _ -> trial


intendedDuration : Trial -> Float
intendedDuration trial =
    case trial.profile of
        Constant -> 600.0

        Linear -> 600.0 + (2200.0 - 600.0) * trial.xFrac

        Quadratic -> -3200.0 * trial.xFrac^2 + 4800 * trial.xFrac + 600.0


intendedArrivalTime : Trial -> Maybe Posix
intendedArrivalTime ({startTime} as trial) =
    let
        st =
            case trial.status of
                Launchable ->
                    Just startTime
                Launching ->
                    Just startTime
                Finished _ ->
                    Just startTime
                _ ->
                    Nothing
    in
        case st of
            Just t ->
                Just <| Time.millisToPosix <| Time.posixToMillis t + (round <| intendedDuration trial)
            Nothing ->
                Nothing


-- SUBSCRIPTIONS


subscriptions : ProgramState -> Sub Msg
subscriptions programState =
    Sub.batch
        [ rocketAnimationSub programState
        , onKeyPress keyDecoder
        , onResize (\w h -> HeightChanged h)
        ]

rocketAnimationSub : ProgramState -> Sub Msg
rocketAnimationSub programState =
    case programState of
        Running ({currentTrial} as state) ->
            case currentTrial.status of
                Launching ->
                    onAnimationFrame StepRocket
                Finished True ->
                    onAnimationFrame StepRocket
                _ ->
                    Sub.none
        _ ->
            Sub.none

keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map filterSpace <| Decode.field "key" Decode.string


filterSpace : String -> Msg
filterSpace str =
    case str of
        " " ->
            SpacePressed

        _ ->
            NoOp



-- VIEW


view : ProgramState -> Html Msg
view programState =
    layout
        [ Font.family
            [ Font.typeface "Lato"
            , Font.sansSerif
            ]
        , Font.color lightgrey
        , Background.color background
        ]
        ( case programState of
            WelcomeState  doDebug debugLog ->
                viewWelcome doDebug debugLog

            Running state ->
                row
                    [ height fill
                    , width fill
                    , inFront (menuPanel state)
                    ]
                    [ viewDebugLog state.debug state.debugLog
                    , column
                        [ alignLeft
                        , centerY
                        , height fill
                        , width <| fillPortion 6
                        -- , explain Debug.todo
                        ]
                        [ drawScreen state
                        ]
                    , column
                        [ width fill
                        , height fill
                        ]
                        [ menuButton
                            lightgrey
                            [ alignTop
                            , alignRight
                            ]
                            MenuOpen
                        ]
                    ]
        )


viewDebugLog : Bool -> List String -> Element Msg
viewDebugLog doDebug log =
    column
        [ centerX
        , centerY
        , height fill
        , width fill
        ]
        [ ( if doDebug then
            column
                [ scrollbarY
                , scrollbarX
                , centerY
                , height fill
                -- , width <| px 300
                , width fill
                , padding 10
                ] <| viewDebugEntries log
            else
                none
            )
        -- , checkbox
        --     [ alignBottom
        --     , padding 40
        --     ]
        --     { onChange = SetDebugging
        --     , icon = defaultCheckbox
        --     , checked = doDebug
        --     , label = labelRight [] <| text "show debugging trace"
        --     }
        ]

viewDebugEntries : List String -> List (Element Msg)
viewDebugEntries log =
    case log of
        [] -> []

        msg::msgs ->
            text msg :: viewDebugEntries msgs


viewWelcome : Bool -> List String -> Element Msg
viewWelcome doDebug debugLog = 
    row
        [ width fill
        , height fill
        ]
        [ viewDebugLog doDebug debugLog
        , column
            [ centerX
            , centerY
            , height fill
            , width <| fillPortion 3
            , padding 75
            ]
            [ heightFillerW 3
            , el
                [ centerX
                , Font.light
                , Font.size 32
                -- , Font.variant Font.smallCaps
                ]
               <| text <| "the model uncertainty experiment"
            , heightFillerW 1
            , row
                [ centerX
                , alignBottom
                , spacing 85
                , padding 25
                ]
                [ button
                   [ Border.width 1
                   , Border.color grey
                   , padding 5
                   , Font.semiBold
                   ]
                   { onPress = Just MakeTrial
                   , label = text "Start"
                   }
                ]
            , heightFillerW 2
            ]
        , column [width fill] []
        ]


drawScreen : ExperimentState -> Element Msg
drawScreen state = 
    let 
        -- screenWidth = 960
        screenWidth = state.width
        -- screenHeight = 740
        screenHeight = state.height
    in
        el
            [ centerX
            , centerY
            ]
            <| html <| svg
                [ SA.width <| fromFloat screenWidth
                , SA.height <| fromFloat screenHeight
                , SA.viewBox <| "0 0 " ++ fromFloat screenWidth ++ " " ++ fromFloat screenHeight
                , SA.fill "rgb(51,51,51)"
                -- , centerX
                ]
                <| Svg.rect
                    [SA.width <| fromFloat screenWidth
                    , SA.height <| fromFloat screenHeight
                    ]
                    [Svg.text "foo"]
                :: drawCue state
                ++ drawFeedback state
                ++ drawTarget state
                ++ drawRocket state


drawRocket : ExperimentState -> List (Svg Msg)
drawRocket ({currentTrial} as state) =
    let
        rocket =
            { x = state.currentTrial.xFrac * state.width
            , y = getRocketY currentTrial.rocketY state
            , r = getRocketRadius state
            }
    in
        case state.currentTrial.status of
            Launching ->
                R.view rocket

            Finished _ ->
                R.view rocket

            _ -> []


drawCue : ExperimentState -> List (Svg Msg)
drawCue state = 
    if state.cue then
        Svg.rect
            [ SA.x <| fromFloat <| getCueX state
            , SA.y <| fromFloat <| getStartY state
            , SA.width <| fromFloat <| getCueWidth state
            , SA.height <| fromFloat <| getCueHeight state
            , SA.fill "rgb(255,255,255)" 
            ]
            []
        :: []
    else
        []


pointsToString : List (Float,Float) -> String
pointsToString points =
    case points of
        [] ->
            ""

        (x,y)::ps ->
            fromFloat x ++ "," ++ fromFloat y ++ " " ++ pointsToString ps


drawTarget : ExperimentState -> List (Svg Msg)
drawTarget state =
    if state.target then
        Svg.polygon
            [ SA.points <| pointsToString <|
                makeCrossPolygonPoints (getX state, getTargetY state) (getTargetSize state)
            , SA.stroke "rgb(255,255,255)"
            , SA.strokeWidth <| fromFloat <| (getTargetSize state) / 4
            ]
            []
        :: []
    else
        []


drawFeedback : ExperimentState -> List (Svg Msg)
drawFeedback ({currentTrial} as state) =
        case currentTrial.status of
            Finished False ->
                if currentTrial.result then
                    Svg.polyline
                        [ SA.points <| pointsToString <|
                            makeCheckmarkPolygonPoints (state.width/2, state.height/2) (getFeedbackSize state)
                        , SA.stroke "rgb(0,255,0)"
                        , SA.strokeWidth <| fromFloat <| (getFeedbackSize state) / 4
                        ]
                        []
                    :: []
                else
                    let
                        err = fromInt <| abs <| round currentTrial.error
                        suffix =
                           if currentTrial.error < 0 then
                               "early"
                           else
                               "late"
                        size = getFeedbackSize state
                    in
                    Svg.polyline
                        [ SA.points <| pointsToString <|
                            makeCrossPolygonPoints (state.width/2, state.height/2) size
                        , SA.stroke "rgb(255,0,0)"
                        , SA.strokeWidth <| fromFloat <| size / 4
                        ]
                        []
                    :: Svg.text_
                            [ SA.x <| fromFloat (state.width/2 - size * 1.4)
                            , SA.y <| fromFloat (state.height/2 + 1.5 * size)
                            , SA.fontFamily "Lato light"
                            , SA.fontSize <| fromFloat (size * 0.4)
                            , SA.fill "rgb(255,0,0)"
                            --, fontStyle "italic"
                            ]
                            [ Svg.text <| err ++ " ms too " ++ suffix
                            ]
                    :: []

            _ ->
                []

makeCrossPolygonPoints : (Float, Float) -> Float -> List (Float,Float)
makeCrossPolygonPoints (x,y) w =
    [ (x,y)
    , (x+w/2, y+w/2)
    , (x,y)
    , (x+w/2, y-w/2)
    , (x,y)
    , (x-w/2, y-w/2)
    , (x,y)
    , (x-w/2,y+w/2)
    ]

makeCheckmarkPolygonPoints : (Float, Float) -> Float -> List (Float, Float)
makeCheckmarkPolygonPoints (x,y) w =
    [ (x-w/2,y-w/2)
    , (x,y)
    , (x+w, y-w)
    ]


getX : ExperimentState -> Float
getX state =
    state.currentTrial.xFrac * state.width


getCueX : ExperimentState -> Float
getCueX state =
    getX state - (getCueWidth state) / 2


getStartY : ExperimentState -> Float
getStartY state =
    (1 - state.params.startYFrac) * state.height


getTargetY : ExperimentState -> Float
getTargetY state =
    (1 - state.params.targetYFrac) * state.height


getRocketY : Float -> ExperimentState -> Float
getRocketY y state =
    let
        flightDist = (state.params.targetYFrac - state.params.startYFrac)
    in
        state.height * (1 - (state.params.startYFrac + flightDist * y))


getRocketRadius : ExperimentState -> Float
getRocketRadius state =
    state.params.rocketSizeFrac * state.height


getCueWidth : ExperimentState -> Float
getCueWidth state =
    state.params.cueWidthFrac * state.height


getCueHeight : ExperimentState -> Float
getCueHeight state =
    (getCueWidth state) / 5


getTargetSize : ExperimentState -> Float
getTargetSize state =
    state.params.targetSizeFrac * state.height


getFeedbackSize : ExperimentState -> Float
getFeedbackSize state =
    state.params.feedbackSizeFrac * state.height


heightFillerW weight =
    el
        [ height <| fillPortion weight
        ]
        none


background = rgb 0.2 0.2 0.2
grey = rgb 0.6 0.6 0.6
lightgrey = rgb 0.8 0.8 0.8


propertySlider : (Float,Float) -> String -> Float -> (Float -> Msg) -> Element Msg
propertySlider (minV, maxV) prop val msgFn = 
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
            { onChange = msgFn
            , label = labelBelow []
                        <| text <| prop ++ ": " ++ fromFloat val
            , min = minV
            , max = maxV
            , value = val
            , thumb = defaultThumb
            , step = Just 1
            }


menuButton : Color -> List (Attribute msg) -> msg -> Element msg
menuButton buttonColor attrs msg =
    let
        radius = 6
        dot : Int -> Element msg
        dot r = 
            el
                [ Border.rounded r
                , width <| px r
                , height <| px r
                , Background.color buttonColor
                ]
                none
    in
        column
            ( [ padding <| 2 * radius
              , spacing radius
              , onClick msg
              ]
              ++ attrs
            )
            <| List.repeat 3 (dot radius)


-- menuPanel : List (Attribute msg) -> List (Element msg) -> Element msg -> Element msg
menuPanel : ExperimentState -> Element Msg
menuPanel state =
    if state.menu then
        el
            [ width fill
            , height fill
            -- , Background.color <| rgba 0 0 0 0.5
            , onClick MenuClose
            , inFront
                <| column
                    [ Border.rounded 6
                    , Background.color lightgrey
                    , Font.color background
                    -- , padding 10
                    , alignTop
                    , alignRight
                    ]
                    [ row
                        [ width fill ]
                        [ el
                            [ padding 18
                            , alignLeft
                            , centerY
                            , Font.size 30
                            , Font.light
                            ]
                            <| text "Menu"
                            , menuButton background [alignRight] MenuClose
                        ]
                    , column
                        [ padding 15
                        , spacing 15
                        ]
                        [ button
                            [centerX]
                            { onPress = Just <| SetDebugging (not state.debug)
                            , label = text
                                ( if state.debug then
                                    "Hide debugging"
                                else
                                    "Show debugging"
                                )
                            }
                        , button
                            [centerX]
                            { onPress = Nothing
                            , label = text "Reset"
                            }
                        , button
                            [centerX]
                            { onPress = Nothing
                            , label = text "Save"
                            }
                        , el
                            [ width fill
                            , Border.width 1
                            , Border.color grey
                            ]
                            none
                        , row
                            [ width fill
                            , spacing 20
                            ]
                            [ column
                                [ alignLeft
                                , Font.light
                                , spacing 15
                                ]
                                [ text "Trial index:"
                                , text "Session performance:"
                                ]
                            , column
                                [ alignRight
                                , Font.medium
                                , spacing 15
                                ]
                                [ text
                                    <| (fromInt (trialIdx state))
                                    ++ " of " ++ (fromInt state.params.nTrials)
                                , text "##%"
                                ]
                            ]
                        ]
                    ]
            ]
            none
    else
        none
