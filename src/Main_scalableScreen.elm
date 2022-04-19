module Main_scalableScreen exposing (..)

import Browser
import Browser.Events exposing (onKeyPress, onAnimationFrameDelta)
import Browser.Navigation exposing (Key)
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
import Svg exposing (Svg, svg)
import Svg.Attributes as SA

import Rocket as R

import String exposing (fromInt, fromFloat)
import Random
import Time exposing (Posix, now)
import Task exposing (perform, andThen)
import Process



-- TODO


type Todo = C String | P String (List Todo)

todos : List String -> List Todo
todos = List.map C

todo : List Todo
todo =
    [ P "side menu bar"
        <| todos
            [ "Show menu dots in upper right corner"
            , "Show save button in side menu"
            , "Show success percentage in side menu"
            , "Show trialIdx in side menu"
            ]
    , P "Welcome screen"
        <| todos
            [ "Replace button with 'press space to start'"
            ]
    , C "Make screen width a function of screen height"
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
    = WelcomeState String
    | Running ExperimentState


type alias ExperimentState =
        { params : Params
        , width : Float
        , height : Float
        , drawBorders : Bool
        , results : List Trial
        , currentTrial : Trial
        -- , status : Status
        , distModel : DistModel
        -- , rocketY : Maybe Float
        , debugLog : List String
        , cue : Bool
        , target : Bool
        , debug : Bool
        }


type alias Params =
    { trialDelayMin : Int
    , trialDelayMax : Int
    , cueDuration : Float
    , targetDuration : Float
    , nTrials : Int
    -- , stepY : Float
    , flightDuration : Float
    , stepT : Int
    , rocketSizeFrac : Float
    , targetSizeFrac : Float
    , cueWidthFrac : Float
    , startYFrac : Float
    , targetYFrac : Float
    }


defaultParams : Params
defaultParams =
    { trialDelayMin = 150
    , trialDelayMax = 1000
    , cueDuration = 35.0
    , targetDuration = 150.0
    , nTrials = 80
    -- , stepY = 0.1
    , flightDuration = 300.0
    , stepT = 1
    , rocketSizeFrac = 0.0075
    , targetSizeFrac = 0.0075
    , cueWidthFrac = 0.0375
    , startYFrac = 0.25
    , targetYFrac = 0.75
    }


type alias Trial =
    { xFrac : Float
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
    | Finished


type Profile
    = Constant
    | Linear
    | Quadratic


-- type Status
--     = Idle
--     | InitialWait
--     | Launchable
--     | Launching
--     | Finished Bool


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
        Finished -> "finished"
        

type alias DistModel = Random.Generator Profile


init : () -> (ProgramState, Cmd Msg)
init _ = (WelcomeState "", Cmd.none)


initExperiment x =
    { params = defaultParams
    , results = []
    , width = 960
    , height = 740
    , drawBorders = False
    -- , status = Idle
    , currentTrial = newTrial x
    , distModel = Random.weighted (100, Constant) []
    -- , rocketY = Just 480
    , debugLog = []
    , cue = False
    , target = False
    , debug = True
    }


-- status : Trial -> Status
-- status t =
--         if t.initialDelay == Nothing then
--             Idle
--         else if t.startTime == Nothing then
--             InitialWait
--         else if t.launchTime == Nothing then
--             Launchable
--         else if t.arrivalTime == Nothing then
--             Launching
--         else
--             Finished <| result t


result : Trial -> Params -> Bool
result trial params =
    case trial.status of
        Finished ->
            let
                duration = Time.posixToMillis trial.arrivalTime - Time.posixToMillis trial.startTime
                error = duration - (round <| intendedDuration trial)
                slack = round(params.targetDuration / 2)
            in
                (abs error) <= slack

        _ ->
            False



-- UPDATE


type Msg
    = MakeTrial
    | SpacePressed
    | PrepTrial Float
    | StartTrial Posix
    | FinishTrial Posix
    | HideCue
    | ShowTarget
    | HideTarget
    | LaunchRocket Posix
    | StepRocket Float
    | SampleX Float
    | SetWidth Float
    | SetHeight Float
    | HideBorders
    | Debug String
    | DoDebug Bool
    | NoOp


update : Msg -> ProgramState -> ( ProgramState, Cmd Msg )
update msg programState =
    case programState of
        WelcomeState debugLog -> 
            case msg of
                MakeTrial ->
                    ( programState
                    , Random.generate SampleX (Random.float 0 1)
                    )

                SampleX x ->
                    ( Running <| initExperiment x
                    , Cmd.none
                    )

                Debug str ->
                    ( WelcomeState <| debugLog ++ str
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
              | debugLog = state.debugLog
                      ++ ["MakeTrial [trialIdx="++(fromInt <| trialIdx state)++"]"]
              }
            , Random.generate SampleX (Random.float 0 1)
            )

        SampleX x ->
            ( { state
              | currentTrial = newTrial x
              , debugLog = state.debugLog ++ ["sampleX [x="++fromFloat x++"]"]
              }
            , Cmd.none
            )

        SpacePressed ->
            case currentTrial.status of
                Idle ->
                    ( { state | debugLog = state.debugLog ++ ["SpacePressed "] }
                    , Random.generate PrepTrial (Random.float 150.0 1000.0)
                    )
                    -- , Task.perform PrepTrial Time.now

                Launchable ->
                    ( { state
                      | debugLog = state.debugLog ++ ["SpacePressed"]
                      }
                    , Task.perform LaunchRocket Time.now
                    )

                _ ->
                    ( state, Cmd.none )

        PrepTrial t ->
            ( { state
              | currentTrial = prepareTrial t state.currentTrial
              , debugLog = state.debugLog ++ ["PrepTrial [delay=" ++ (fromFloat t) ++ "] "]
              }
            , Task.perform StartTrial (Process.sleep t |> andThen (\_ -> now))
            )

        StartTrial time ->
            ( { state
              | currentTrial = startTrial time state.currentTrial
              , cue = True
              , debugLog = state.debugLog
                      ++ [ "StartTrial [intendedDuration=" ++ (fromFloat <| intendedDuration
                      state.currentTrial) ++ "] "]
              }
            , Task.perform (\_ -> HideCue) (Process.sleep state.params.cueDuration)
            )

        HideCue ->
            ( { state
              | cue = False
              , debugLog = state.debugLog ++ ["HideCue"]
              }
            , Task.perform
                (\_ -> ShowTarget)
                (Process.sleep <| (intendedDuration state.currentTrial) - state.params.cueDuration )
            )

        ShowTarget ->
            ( { state
              | target = True
              , debugLog = state.debugLog ++ ["ShowTarget "]
              }
            , Task.perform (\_ -> HideTarget) (Process.sleep state.params.targetDuration)
            )

        HideTarget ->
            ( { state
              | target = False
              , debugLog = state.debugLog ++ ["HideTarget"]
              }
            , Cmd.none
            )

        LaunchRocket time ->
            ( { state
              | currentTrial = launchTrial time state.currentTrial
              , debugLog = state.debugLog ++ ["LaunchRocket"]
              }
            , Cmd.none
            )

        StepRocket dt ->
            let
                updatedTrial =
                    { currentTrial
                    | rocketY =
                        currentTrial.rocketY + (dy dt state.params)
                    }
            in
                case currentTrial.status of
                    Launching ->
                        ( { state
                          | currentTrial = updatedTrial
                          -- , debugLog = state.debugLog ++ ["StepRocket"]
                          }
                        , if updatedTrial.rocketY >= 1.0 && state.target ||
                            updatedTrial.rocketY >= 1.7 then
                            Task.perform FinishTrial Time.now
                          else
                              Cmd.none
                        )

                    _ ->
                        ( state, Cmd.none )

        FinishTrial time ->
            case currentTrial.status of
                Launching ->
                    let
                        updatedTrial = finishTrial time currentTrial
                    in
                        ( { state
                          | currentTrial = updatedTrial
                          , results = state.results ++ [updatedTrial]
                          , debugLog =
                              state.debugLog
                              ++ [ "finishTrial [result="
                                  ++ (if result updatedTrial state.params then
                                      "Success"
                                      else "Failure")
                                    ++ "]"
                                ]
                          }
                        , Task.perform (\_ -> MakeTrial) (Process.sleep 1500.0)
                        )

                _ ->
                    ( state, Cmd.none )

        SetWidth w ->
            ( { state
              | width = w
              , drawBorders = True
              }
            , Task.perform (\_ -> HideBorders) (Process.sleep 80.0)
            )

        SetHeight h ->
            ( { state
              | height = h
              , drawBorders = True
              }
            , Task.perform (\_ -> HideBorders) (Process.sleep 80.0)
            )

        HideBorders ->
            ( { state
              | drawBorders = False
              }
            , Cmd.none
            )

        DoDebug bool ->
            ( { state | debug = bool }
            , Cmd.none
            )

        NoOp ->
            ( state, Cmd.none )


dy : Float -> Params -> Float
dy dt p = dt / p.flightDuration

newTrial : Float -> Trial
newTrial x =
    { startTime = Time.millisToPosix -1
    , launchTime = Time.millisToPosix -1
    , arrivalTime = Time.millisToPosix -1
    , initialDelay = -1
    , xFrac = x
    , profile = Constant
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


finishTrial : Posix -> Trial -> Trial
finishTrial time trial =
    case trial.status of
        Launching ->
            { trial
            | arrivalTime = time
            , status = Finished
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
                Finished ->
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
    case programState of
        Running state ->
            case state.currentTrial.status of
                Launching ->
                    onAnimationFrameDelta StepRocket
                _ ->
                    onKeyPress <| keyDecoder

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
            WelcomeState debugLog ->
                viewWelcome debugLog

            Running state ->
                row
                    [ centerY
                    , alignLeft
                    , height fill
                    , width fill
                    -- , padding 40
                    ]
                    [ column
                        [ centerX
                        , centerY
                        , height fill
                        , width fill
                        ]
                        [ ( if state.debug then
                                column
                                    [ scrollbarY
                                    , scrollbarX
                                    , centerY
                                    , height <| px (round state.height)
                                    , width <| px 300
                                    , padding 10
                                    ] <| viewDebugLog state.debugLog
                            else
                                none
                          )
                        , checkbox
                            [ alignBottom
                            , padding 40
                            ]
                            { onChange = DoDebug
                            , icon = defaultCheckbox
                            , checked = state.debug
                            , label = labelRight [] <| text "show debugging trace"
                            }
                        ]
                    , column
                        [ alignLeft
                        , centerY
                        , height fill
                        , width <| fillPortion 6
                        -- , explain Debug.todo
                        ]
                        [ drawScreen state
                        , row
                            [ alignLeft
                            , alignBottom
                            , spacing 85
                            , padding 20
                            ]
                            [ propertySlider (500, 1600) "Width" state.width SetWidth
                            , propertySlider (500, 850) "Height" state.height SetHeight
                            ]
                        ]
                    , column
                        [ width fill ]
                        []
                    ]
        )


viewDebugLog : List String -> List (Element Msg)
viewDebugLog log =
    case log of
        [] -> []

        msg::msgs ->
            text msg :: viewDebugLog msgs


viewWelcome : String -> Element Msg
viewWelcome debugLog = 
    column
        [ centerX
        , centerY
        , height fill
        , width fill
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
        , text debugLog
        , heightFillerW 2
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
            ( [ centerX
              , centerY
              ]
              ++ (if state.drawBorders then
                    [ Border.width 1
                    , Border.color lightgrey
                    ]
                else [])
            )
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
                ++ drawTarget state
                ++ drawRocket state


drawRocket : ExperimentState -> List (Svg Msg)
drawRocket ({currentTrial} as state) =
    case state.currentTrial.status of
        Launching ->
            let
                rocket =
                    { x = state.currentTrial.xFrac * state.width
                    , y = getRocketY currentTrial.rocketY state
                    , r = getRocketRadius state
                    }
            in
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


drawTarget : ExperimentState -> List (Svg Msg)
drawTarget state =
    let
        makeTargetPolygonPoints : (Float, Float) -> Float -> List (Float,Float)
        makeTargetPolygonPoints (x,y) w =
            [ (x,y)
            , (x+w/2, y+w/2)
            , (x,y)
            , (x+w/2, y-w/2)
            , (x,y)
            , (x-w/2, y-w/2)
            , (x,y)
            , (x-w/2,y+w/2)
            ]

        pointsToString : List (Float,Float) -> String
        pointsToString points =
            case points of
                [] ->
                    ""

                (x,y)::ps ->
                    fromFloat x ++ "," ++ fromFloat y ++ " " ++ pointsToString ps
    in
    if state.target then
        Svg.polygon
            [ SA.points <| pointsToString <|
                makeTargetPolygonPoints (getX state, getTargetY state) (getTargetSize state)
            , SA.stroke "rgb(255,255,255)"
            , SA.strokeWidth <| fromFloat <| (getTargetSize state) / 4
            ]
            []
        :: []
    else
        []


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


heightFiller =
    el
        [ height fill
        ]
        none


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
