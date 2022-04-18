module Main exposing (..)

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
        , results : List Trial
        , currentTrial : Trial
        -- , status : Status
        , distModel : DistModel
        , rocketY : Maybe Float
        , debugStr : String
        , cue : Bool
        , target : Bool
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
    , cueDuration = 350.0
    , targetDuration = 150.0
    , nTrials = 80
    -- , stepY = 0.1
    , flightDuration = 3000.0
    , stepT = 1
    , rocketSizeFrac = 0.0075
    , targetSizeFrac = 0.0075
    , cueWidthFrac = 0.0375
    , startYFrac = 0.25
    , targetYFrac = 0.75
    }


type alias Trial =
    { xFrac : Float
    , startTime : Maybe Posix
    , launchTime : Maybe Posix
    , arrivalTime : Maybe Posix
    , initialDelay : Maybe Int
    , profile : Profile
    }


type Profile
    = Constant
    | Linear
    | Quadratic


type Status
    = Idle
    | InitialWait
    | Launchable
    | Launching
    | Finished


statusToString : Status -> String
statusToString s =
    case s of
        Idle -> "idle"
        InitialWait -> "initialwait"
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
    -- , status = Idle
    , currentTrial = newTrial x
    , distModel = Random.weighted (100, Constant) []
    , rocketY = Just 480
    , debugStr = "debug: "
    , cue = False
    , target = False
    }


status : Trial -> Status
status t =
        if t.initialDelay == Nothing then
            Idle
        else if t.startTime == Nothing then
            InitialWait
        else if t.launchTime == Nothing then
            Launchable
        else if t.arrivalTime == Nothing then
            Launching
        else
            Finished



-- UPDATE


type Msg
    = MakeTrial
    | SpacePressed
    | PrepTrial Int
    | StartTrial Posix
    | HideCue
    | ShowTarget
    | HideTarget
    | LaunchRocket Posix
    | StepRocket Float
    | SampleX Float
    | SetWidth Float
    | SetHeight Float
    | Debug String
    | NoOp


update : Msg -> ProgramState -> ( ProgramState, Cmd Msg )
update msg programState =
    case programState of
        WelcomeState debugStr -> 
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
                    ( WelcomeState <| debugStr ++ str
                    , Cmd.none
                    )

                _ ->
                    (programState, Cmd.none)

        Running state ->
            (\(newState, cmd) -> (Running newState, cmd)) <| updateExperimentState msg state


updateExperimentState : Msg -> ExperimentState -> (ExperimentState, Cmd Msg)
updateExperimentState msg state =
    case msg of
        Debug str ->
            ( { state | debugStr = state.debugStr ++ str }
            , Cmd.none
            )

        MakeTrial ->
            ( state
            , Random.generate SampleX (Random.float 0 1)
            )

        SampleX x ->
            ( { state | currentTrial = newTrial x }
            , Cmd.none
            )

        SpacePressed ->
            case status state.currentTrial of
                Idle ->
                    ( { state | debugStr = state.debugStr ++ "." }
                    , Random.generate PrepTrial (Random.int 150 1000)
                    )
                    -- , Task.perform PrepTrial Time.now

                Launchable ->
                    ( { state | rocketY = Just 0 }
                    , Task.perform LaunchRocket Time.now
                    )

                _ ->
                    ( state, Cmd.none )

        PrepTrial t ->
            ( { state
              | debugStr = state.debugStr ++ "PrepTrial[" ++ (fromInt t) ++ "]"
              , currentTrial = prepareTrial t state.currentTrial
              }
            , Task.perform StartTrial (Process.sleep (toFloat t) |> andThen (\_ -> now))
            )

        StartTrial time ->
            ( { state
              | currentTrial = startTrial time state.currentTrial
              , cue = True
              , debugStr = state.debugStr ++ "StartTrial [" ++ (fromFloat <| trialDuration  state.currentTrial) ++ "]"
              }
            , Task.perform (\_ -> HideCue) (Process.sleep state.params.cueDuration)
            )

        HideCue ->
            ( { state | cue = False }
            , Task.perform
                (\_ -> ShowTarget)
                (Process.sleep <| (trialDuration state.currentTrial) - state.params.cueDuration )
            )

        ShowTarget ->
            ( { state | target = True }
            , Task.perform (\_ -> HideTarget) (Process.sleep state.params.targetDuration)
            )

        HideTarget ->
            ( { state | target = False }
            , Cmd.none
            )

        LaunchRocket time ->
            ( { state | currentTrial = launchTrial time state.currentTrial }
            , Cmd.none
            )

        StepRocket millis ->
            case state.rocketY of
                Just y ->
                    ( { state | rocketY = Just <| y + millis / state.params.flightDuration }
                    , Cmd.none
                    )
                Nothing ->
                    ( state, Cmd.none )

        SetWidth w ->
            ( { state | width = w }
            , Cmd.none)

        SetHeight h ->
            ( { state | height = h }
            , Cmd.none
            )

        NoOp ->
            ( state, Cmd.none )


newTrial : Float -> Trial
newTrial x =
    { startTime = Nothing
    , launchTime = Nothing
    , arrivalTime = Nothing
    , initialDelay = Nothing
    , xFrac = x
    , profile = Constant
    }


prepareTrial : Int -> Trial -> Trial
prepareTrial delay trial =
    { trial | initialDelay = Just delay }



startTrial : Posix -> Trial -> Trial
startTrial time trial =
    case trial.startTime of
        Nothing ->
            { trial | startTime = Just time }

        Just _ ->
            trial


launchTrial : Posix -> Trial -> Trial
launchTrial time trial =
    case trial.launchTime of
        Nothing ->
            { trial | launchTime = Just time }

        Just _ ->
            trial


trialDuration : Trial -> Float
trialDuration trial =
    case trial.profile of
        Constant -> 600.0

        Linear -> 600.0 + (2200.0 - 600.0) * trial.xFrac

        Quadratic -> -3200.0 * trial.xFrac^2 + 4800 * trial.xFrac + 600.0
            


-- SUBSCRIPTIONS


subscriptions : ProgramState -> Sub Msg
subscriptions programState =
    case programState of
        Running state ->
            if status state.currentTrial == Launching then
                onAnimationFrameDelta StepRocket
            else
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
    case programState of
        WelcomeState debugStr ->
            viewWelcome debugStr

        Running state ->
            layout
                []
                <| column
                    [ centerX
                    , centerY
                    , height fill
                    , width fill
                    , padding 75
                    -- , explain Debug.todo
                    ]
                    [ heightFiller
                    , drawScreen state
                    , heightFiller
                    , el [centerX] <| text <| "[" ++ state.debugStr ++ "]"
                   -- , el
                   --     [ centerX
                   --     , alignBottom
                   --     ]
                   --     <| text <| (statusToString << status) state.currentTrial
                   --     ++ "[" ++ fromFloat <| getX state ++ ", "
                   --     ++ ( case state.rocketY of
                   --             Just y -> fromFloat y
                   --             Nothing -> ""
                   --         )
                   --     ++ "] " ++ "cue: " ++ (if state.cue then "true" else "false")
                    , row
                        [ centerX
                        , alignBottom
                        , spacing 85
                        ]
                        [ propertySlider (500, 1500) "Width" state.width SetWidth
                        , propertySlider (500, 1500) "Height" state.height SetHeight
                        ]
                    ]


viewWelcome : String -> Html Msg
viewWelcome debugStr = 
    layout
        [ Font.family
            [ Font.typeface "Lato"
            ]
        ]
        <| column
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
            , text debugStr
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
        el [centerX]
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
                    []
                :: drawCue state
                ++ drawTarget state
                ++ drawRocket state


drawRocket : ExperimentState -> List (Svg Msg)
drawRocket state =
    case state.rocketY of
        Just y ->
            let
                rocket =
                    { x = state.currentTrial.xFrac * state.width
                    , y = getRocketY y state
                    , r = getRocketRadius state
                    }
            in
                R.view rocket

        Nothing -> []


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
                makeTargetPolygonPoints (getX state, getTargetY state) state.params.targetSizeFrac
            , SA.stroke "rgb(255,255,255)"
            , SA.strokeWidth <| fromFloat <| state.params.targetSizeFrac / 4
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
    state.params.cueWidthFrac * state.width


getCueHeight : ExperimentState -> Float
getCueHeight state =
    state.params.cueWidthFrac * state.width / 5


screenX : Float -> ExperimentState -> Float
screenX x state =
    x * state.width

screenY : Float -> ExperimentState -> Float
screenY y state =
    (1-y) * state.height


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
