port module Main exposing (..)

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
import Time exposing (Posix, now, millisToPosix, posixToMillis)
import Task exposing (perform, andThen)
import Process
import File.Download as Download

import Json.Decode
import Json.Encode
import Audio exposing
    ( Audio
    , AudioCmd
    , AudioData
    , audio
    , silence
    , elementWithAudio
    )


-- TODO


type Todo
    = P String (List Todo)
    | Do Urgency String
    | Doing String
    | Done String
    | Canceled String

type Urgency
    = High
    | Medium
    | Low

todo : List Todo
todo =
    [ P "side menu bar"
        [ Done "overlay elements over other elements (Element.inFront, Element.behindContent)"
        , Done "Show menu dots in upper right corner"
        , Done "Show save button in side menu"
        , Do Low "Show success percentage in side menu"
        , Done "Show trialIdx in side menu"
        , Do Low "reset experiment when reset button is clicked"
        ]
    , P "Data logistics"
        [ Done "Download a string to a file"
        , P "Convert trials to csv"
            [ P "What are the columns in the csv output?"
                [ P "Column1 are the X coordinates"
                    [ Done "What are the boundaries of the x values"
                    , Done "The x values range from -150 to +150"
                    ]
                , Done "Column 2 are the baselines"
                , Done "Column 3 are the produced intervals"
                ]
            , Done "Implement trials2Csv"
            ]
        , Done "Save data when save button is clicked"
        , Do Low "Warn when trying to save an incomplete session"
        , Done "Save each trial to the Experiment results list when creating a new trial"
        , Done "Prompt user to download data at the end of the session"
        ]
    , P "Welcome screen"
            [ Done "Replace button with 'press space to start'"
            , Canceled "Have dropdowns to choose the session index"
            , Do Low "Animate (i.e. fade in/out) the 'press space to start' string"
            , P  "Show help"
                [ Done "Show help button on welcome screen"
                , Done "Show popup when 'help' is clicked"
                , Do Medium "Write help text in popup"
                ]
            , Do Low "Make a demo mode to explain the experiment"
            , Done "Make two buttons to allow choosing Train vs Test session"
            ]
    , P "Experiment logistics"
            [ P "Make sure the experiment has the correct no. trials"
                [ Done "On mixture sessions, start with 50 trials of 'baseline'"
                , Done "What is the model type during the baseline of mixture sessions?"
                , Done "The baseline is quadratic"
                , Done "On mixture sessions, have 100 trials per mixture condition"
                ]
            , Done "Don't respond to multiple space presses simultaneously"
            , P "Sample random Profile for each trial"
                [ P "When updating with the MakeTrial message -> Sample random profile and send it to 'newTrial"
                    [ Done "Check if its possible to chain the commands for sampling a profile and samping the x coordinate together"
                    , Canceled "Use Cmd.batch to sample both the x coord and the profile"
                    , Done "Chain the sampling of the profile and the x coord after each other"
                    ]
                ]
            , P "What is the makeup of the training data? -> Check in matlab"
                [ Done "Determine total no. trials -> 360"
                , Done "ALl three models are in the data"
                , P "Determine order of different models"
                    [ Done "The order is different for different participants"
                    , Done "The training consists of three stages of 120 trials with only unmixed models"
                    , Do Low "There are in fact six different ways of making training sessions (with the models in a different order) -> Check with Devika whether these should be implemented or if a single type of training session is sufficient"
                    ]
                ]
            , P "Enable the experiment with the different models"
                [ P "Implement the order and composition of the different models"
                    [ Done "The mixtures are LC (linear+const) and QC (quadratic+const)"
                    , Done "The test experiment consists of six sessions"
                    , Done "QC1-3 followed by LC1-3, with Q:C and L:C as given by"
                    , Done "QC1 -> 15:85, QC2 -> 65:35, QC3 -> 50:50"
                    , Done "LC1 -> 50:50, LC2 -> 65:35, LC3 -> 85:15"
                    ]
                ]
            , P "Make sure the right participant has the correct generative model in each session"
                [ Do Low "Make an input/dropdown field for participants to have an id"
                , Done "Map session index to generative model"
                , Canceled <| "Make a counter/dropdown to select the index of the"
                    ++ "session -> each session index corresponds to a generative"
                    ++ "model in a deterministic manner"
                ]
            , P "Give task feedback after each trial"
                [ Canceled "Only show target when it is hit"
                , Done "Freeze the whole experiment at the moment the trial has elapsed"
                , P "Assign points on successful trials"
                    [ Done "exponential within eligibility window"
                    , Done "Show cumulative points on the top of the screen"
                    ]
                , P "keep both rocket and target visible after hit until the next trial initiates"
                    [ Done "Refactor StepRocket to take an absolute time instead of a delta"
                    , Done "Refactor the update StepRocket to Finish the trial internally if it reaches the target height"
                    ]
                , P "make indicator for trial failure"
                    [ Done "Show cross icon"
                    , Done "Write feedback about the error"
                    , Do Low "Align centers of feedback text and cross icon (Svg magic)"
                    ]
                , P "make indicator for trial success"
                    [ Done "Show green checkmark"
                    , Done "Ring a sound on target hit"
                    , Done "Show awardeded points"
                    ]
                ] 
            , P "Make the experiment screen the proper size"
                [ P "Make screen width a function of screen height"
                    [ Done "initialize screen height: getViewPort : Viewport.viewport.height : Float"
                    , Done "subscribe to screen size changes -> Browser.Events.onResize"
                    ]
                , Do Low "Constrain screen width such that it always fits on the page"
                ]
            ]
    , P "Debug Log"
        [ Done "Show new debug messages at the top -> append to head of debugLog"
        , Do Low "Make debugLog a floating object"
        ]
    ]



-- MAIN


main : Platform.Program () (Audio.Model Msg ProgramState) (Audio.Msg Msg)
main =
     Audio.elementWithAudio
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , audio = renderAudio
        , audioPort = { toJS = audioPortToJS, fromJS = audioPortFromJS }
        }

port audioPortToJS : Json.Encode.Value -> Cmd msg


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg



-- MODEL


type ProgramState
    = Initializing SetupState
    | Running ExperimentState


type alias SetupState =
    { debug : Bool
    , debugLog : List String
    , sessionType : SessionType
    , showHelp : Bool
    , beep : Maybe Audio.Source
    }


type alias ExperimentState =
        { sessionType : SessionType
        , height : Float
        , width : Float 
        , results : List TrialData
        , currentTrial : Trial
        , debugLog : List String
        , cue : Bool
        , target : Bool
        , debug : Bool
        , menu : Bool
        , totalScore : Int
        , beep : Maybe Audio.Source
        , beepAt : Maybe Posix
        }


init : flags -> (ProgramState, Cmd Msg, AudioCmd Msg)
init _ =
    ( Initializing
        { debug = False
        , debugLog = []
        , sessionType = Train
        , showHelp = False
        , beep = Nothing
        }
    , Cmd.none
    , Audio.loadAudio
        SoundLoaded
            "beep.wav"
    )

viewport2HeightChanged : Viewport -> Msg
viewport2HeightChanged v =
    let 
        width = round v.viewport.width
        height = round v.viewport.height
    in
        HeightChanged height


initExperiment : ProgramState -> Profile -> Float -> ProgramState
initExperiment programState profile x =
    case programState of
        Initializing state ->
            Running
                { height = 740
                , width = 960
                , results = []
                , currentTrial = newTrial profile x
                , debugLog = state.debugLog
                , cue = False
                , target = False
                , debug = state.debug
                , menu = False
                , sessionType = state.sessionType
                , totalScore = 0
                , beep = state.beep
                , beepAt = Nothing
                }

        _ -> programState


type alias Params =
    { trialDelayMin : Int
    , trialDelayMax : Int
    , cueDuration : Float
    , targetDuration : Float
    , iti : Float
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


params : Params
params =
    { trialDelayMin = 150
    , trialDelayMax = 1000
    , cueDuration = 35.0
    , targetDuration = 150.0
    , iti = 1500.0
    , flightDuration = 300.0
    , stepT = 1
    , rocketSizeFrac = 0.01
    , targetSizeFrac = 0.015
    , feedbackSizeFrac = 0.055
    , cueWidthFrac = 0.06
    , startYFrac = 0.25
    , targetYFrac = 0.75
    , aspectRatio = 8/6
    }


type Trial
    = Idle
        { xFrac : Float
        , profile : Profile
        }
    | Waiting
        { xFrac : Float
        , profile : Profile
        , initialDelay : Float
        }
    | Launchable
        { xFrac : Float
        , profile : Profile
        , initialDelay : Float
        , startTime : Posix
        }
    | Launched TrialData


type alias TrialData =
    { xFrac : Float
    , profile : Profile
    , initialDelay : Float
    , startTime : Posix
    , currentTime : Posix
    , launchTime : Posix
    }


trialIdx : ExperimentState -> Int
trialIdx state =
    state.results |> List.length |> (+) 1


statusToString : Trial -> String
statusToString trial =
    case trial of
        Idle _ -> "idle"
        Waiting _ -> "waiting"
        Launchable _ -> "launchable"
        Launched _ -> "launched"
        

type SessionType
    = Train
    | Test


nTrials : SessionType -> Int
nTrials s =
    case s of
        Train -> 5--360
        Test -> 650


sessionComplete : ExperimentState -> Bool
sessionComplete state =
    trialIdx state > nTrials state.sessionType


type ModelID
    = C
    | L
    | Q
    | QC1
    | QC2
    | QC3
    | LC1
    | LC2
    | LC3


getDistModel : ModelID -> DistModel
getDistModel id =
    case id of
        C -> Random.weighted (100, Constant) []
        L -> Random.weighted (100, Linear) []
        Q -> Random.weighted (100, Quadratic) []
        QC1 -> Random.weighted (85,Quadratic) [(15,Constant)]
        QC2 -> Random.weighted (65,Quadratic) [(35,Constant)]
        QC3 -> Random.weighted (50,Quadratic) [(50,Constant)]
        LC1 -> Random.weighted (50,Linear) [(50,Constant)]
        LC2 -> Random.weighted (65,Linear) [(35,Constant)]
        LC3 -> Random.weighted (85,Linear) [(15,Constant)]


type Profile
    = Constant
    | Linear
    | Quadratic


profile2Str : Profile -> String
profile2Str p =
    case p of
        Constant -> "Constant"
        Linear -> "Linear"
        Quadratic -> "Quadratic"


type alias DistModel = Random.Generator Profile

getModelId : Int -> SessionType -> ModelID
getModelId iTr sType =
    case sType of
        Train ->
            if iTr <= 120 then
                C
            else if iTr <= 240 then
                L
            else --if iTr <= 360 then
                Q

        Test ->
            if iTr <= 50 then
                Q
            else if iTr <= 150 then
                QC1
            else if iTr <= 250 then
                QC2
            else if iTr <= 350 then
                QC3
            else if iTr <= 450 then
                LC1
            else if iTr <= 550 then
                LC2
            else --if iTr <= 650 then
                LC3


--type alias DistModel = List (Profile, Int)



-- UPDATE


type Msg
    = ShowHelp
    | SetDebugging Bool
    | HeightChanged Int
    | MenuOpen
    | MenuClose
    | DownloadResults
    | Debug String
    | NoOp
    | SoundLoaded (Result Audio.LoadError Audio.Source)
    | SetSessionType SessionType
    | SpacePressed
    | NextTrial
    | RandomDelay Float
    | RandomProfile Profile
    | RandomProfileAndX Profile Float
    | StartTrial Posix
    | HideCue
    | LaunchRocket Posix
    | StepRocket Posix
    | ShowTarget Posix


update : AudioData -> Msg -> ProgramState -> (ProgramState, Cmd Msg, AudioCmd Msg)
update _ msg programState =
    case programState of
        Running state ->
            (\(newState, cmd) -> (Running newState, cmd, Audio.cmdNone)) <| updateExperimentState msg state

        Initializing state -> 
            case msg of
                RandomProfileAndX profile x ->
                    ( initExperiment programState profile x
                    , Task.perform viewport2HeightChanged getViewport
                    , Audio.cmdNone
                    )

                SoundLoaded soundData ->
                    case soundData of
                        Ok sound ->
                            ( Initializing { state | beep = Just sound }
                            , Cmd.none
                            , Audio.cmdNone
                            )

                        Err _ ->
                            ( programState
                            , Cmd.none
                            , Audio.cmdNone
                            )

                _ ->
                    (\(newState, cmd) -> (Initializing newState, cmd, Audio.cmdNone)) <| updateSetupState msg state


updateSetupState : Msg -> SetupState -> (SetupState, Cmd Msg)
updateSetupState msg state =
    case msg of
        ShowHelp ->
            ( { state | showHelp = not state.showHelp }
            , Cmd.none
            )

        SetSessionType sType ->
            ( { state | sessionType = sType }
            , Cmd.none
            )

        NextTrial ->
            ( state
            , Random.generate
                RandomProfile
                (getDistModel <| getModelId 1 state.sessionType)
            )

        RandomProfile profile ->
            ( state
            , Random.generate (RandomProfileAndX profile) (Random.float 0 1)
            )

        SpacePressed ->
            ( state
            , Task.perform (\_ -> NextTrial) Time.now
            )

        SetDebugging bool ->
            ( { state | debug = bool }
            , Cmd.none
            )

        Debug str ->
            ( { state | debugLog = str::state.debugLog }
            , Cmd.none
            )

        _ ->
            (state, Cmd.none)


updateExperimentState : Msg -> ExperimentState -> (ExperimentState, Cmd Msg)
updateExperimentState msg ({currentTrial} as state) =
    case msg of
        Debug str ->
            ( { state | debugLog = state.debugLog ++ [str] }
            , Cmd.none
            )

        NextTrial ->
            case currentTrial of
                Launched trialData ->
                    ( { state
                      | target = False
                      , results = state.results ++ [trialData]
                      , totalScore = state.totalScore + (score << error) trialData
                      , debugLog =
                            ("MakeTrial [trialIdx="++(fromInt <| trialIdx state)++"]")::state.debugLog
                      }
                    , Random.generate
                        RandomProfile
                        (getDistModel <| getModelId (trialIdx state) state.sessionType)
                    )

                _ -> (state, Cmd.none)

        RandomProfile profile ->
            ( { state
              | debugLog = 
                    ("RandomProfile ["++profile2Str profile++"]")::state.debugLog
              }
            , Random.generate (RandomProfileAndX profile) (Random.float 0 1)
            )

        RandomProfileAndX profile x ->
            ( { state
              | currentTrial = newTrial profile x
              , debugLog = ("sampleX [x="++fromFloat x++"]")::state.debugLog
              }
            , Cmd.none
            )

        SpacePressed ->
            if sessionComplete state then
                ( state
                , Task.perform (\_ -> DownloadResults) Time.now
                )
            else
                case currentTrial of
                    Idle _ ->
                        ( { state | debugLog = "SpacePressed"::state.debugLog
                          }
                        , Random.generate RandomDelay (Random.float 150.0 1000.0)
                        )
                        -- , Task.perform PrepTrial Time.now

                    Launchable _ ->
                        ( { state
                          | debugLog = "SpacePressed"::state.debugLog
                          }
                        , Task.perform LaunchRocket Time.now
                        )

                    _ ->
                        ( state, Cmd.none )

        RandomDelay delay ->
            case currentTrial of
                Idle oldData ->
                    let
                        newData =
                            { xFrac = oldData.xFrac
                            , profile = oldData.profile
                            , initialDelay = delay
                            }
                    in
                        ( { state
                          | currentTrial = Waiting newData
                          , debugLog = ("RandomDelay [" ++ (fromFloat delay) ++ "] ")::state.debugLog
                          }
                        , Task.perform (StartTrial) (Process.sleep delay |> andThen (\_ -> now))
                        )

                _ -> (state, Cmd.none)

        StartTrial startTime ->
            let
                durationStr = fromFloat <| duration state.currentTrial
            in
                ( { state
                  | currentTrial = startTrial startTime state.currentTrial
                  , cue = True
                  , debugLog = ("StartTrial [duration="++durationStr++"]")::state.debugLog
                  }
                , Task.perform (\_ -> HideCue) (Process.sleep params.cueDuration)
                )

        HideCue ->
            ( { state
              | cue = False
              , debugLog = "HideCue"::state.debugLog
              }
            , Task.perform
                ShowTarget
                (Process.sleep
                    ( (duration state.currentTrial)
                      - params.cueDuration
                      - params.targetDuration / 2
                    )
                    |> andThen (\_ -> now)
                )
            )

        ShowTarget targetTime ->
            case currentTrial of
                Launched trialData ->
                    ( { state
                      | target = True
                      , beepAt = 
                            if result trialData then
                                Just targetTime
                            else
                                Nothing
                      , debugLog = "ShowTarget"::state.debugLog
                      }
                    , Task.perform
                        (\_ -> NextTrial)
                        (Process.sleep <| params.targetDuration + params.iti)
                    )

                Launchable _ ->
                    ( { state
                      | target = True
                      , debugLog = "ShowTarget"::state.debugLog
                      }
                    , Cmd.none
                    )

                _ -> (state, Cmd.none)

        LaunchRocket time ->
            ( { state
              | currentTrial = launchTrial time state.currentTrial
              , debugLog = "LaunchRocket"::state.debugLog
              }
            , if state.target then --target is already visible -> start new trial
                Task.perform
                    (\_ -> NextTrial)
                    (Process.sleep <| params.targetDuration + params.iti)
            else
                 Cmd.none
            )

        StepRocket newTime ->
            case currentTrial of
                Launched trialData ->
                    let
                        newData =
                            if (posixToMillis newTime)
                            > (posixToMillis <| endTime trialData) then
                                { trialData | currentTime = endTime trialData }
                            else
                                { trialData | currentTime = newTime }
                    in
                        ( { state
                          | currentTrial = Launched newData
                          , debugLog =
                                ("StepRocket [time="++(fromInt <| posixToMillis newTime)++"]")::state.debugLog
                          }
                        , Cmd.none
                        )

                _ -> (state, Cmd.none)

        HeightChanged h ->
            let
                newWidth = (toFloat h) * params.aspectRatio
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

        DownloadResults ->
            ( state
            , Download.string
                ("modelUncertainty_session="
                ++ case state.sessionType of
                        Train -> "train"
                        Test -> "test"
                )
                "text/csv"
                (trials2Csv state.results)
            )

        _ ->
            ( state, Cmd.none )


trials2Csv : List TrialData -> String
trials2Csv list =
    case list of
        [] -> ""

        tr::trs ->
            String.join
                ","
                [ fromFloat tr.xFrac
                , fromFloat <| duration (Launched tr)
                , fromInt
                    <| (posixToMillis tr.launchTime)
                    + (round params.flightDuration)
                    - (posixToMillis tr.startTime)
                ]


newTrial : Profile -> Float -> Trial
newTrial profile x =
    Idle
        { xFrac = x
        , profile = profile
        }


startTrial : Posix -> Trial -> Trial
startTrial time trial =
    case trial of
        Waiting oldData ->
            let
                newData = 
                    { xFrac = oldData.xFrac
                    , profile = oldData.profile
                    , initialDelay = oldData.initialDelay
                    , startTime = time
                    }
            in
                Launchable newData

        _ ->
            trial


launchTrial : Posix -> Trial -> Trial
launchTrial time trial =
    case trial of
        Launchable oldData ->
            let
                newData =
                    { xFrac = oldData.xFrac
                    , profile = oldData.profile
                    , initialDelay = oldData.initialDelay
                    , startTime = oldData.startTime
                    , currentTime = time
                    , launchTime = time
                    }
            in
                Launched newData

        _ -> trial


error : TrialData -> Int
error trialData =
    (posixToMillis <| endTime trialData) - (posixToMillis <| arrivalTime trialData)


result : TrialData -> Bool
result trialData =
    (abs <| error trialData) <= (round <| params.targetDuration / 2)


score : Int -> Int
score err =
    let
        width = round <| params.targetDuration / 2

        maxPoints = 100

        e : Float
        e = 2.718281828

        log : Float -> Float
        log x =
            let
                p = 0.00001 -- governs precision of approximation
            in
                (x^(p) - 1) / p
    in
    if err < 0 then
        (score << abs) err
    else if err > width then
        0
    else
        round <| maxPoints * ( e ^ (-(toFloat err) * (log maxPoints) / (toFloat width)))


arrivalTime : TrialData -> Posix
arrivalTime trial =
    millisToPosix <| (round params.flightDuration) + (posixToMillis trial.launchTime)


duration : Trial -> Float
duration trial =
    let
        (profile, xFrac) =
            case trial of
                Idle data -> (data.profile, data.xFrac)
                Waiting data -> (data.profile, data.xFrac)
                Launchable data -> (data.profile, data.xFrac)
                Launched data -> (data.profile, data.xFrac)
    in
    case profile of
        Constant -> 1200.0

        Linear -> 1725.0 - 1050 * xFrac

        Quadratic -> -3200.0 * xFrac^2 + 4800 * xFrac + 600.0


endTime : TrialData -> Posix
endTime trialData =
    millisToPosix
        <| (round <| duration (Launched trialData)) + (posixToMillis trialData.startTime)


hasEnded : TrialData -> Bool
hasEnded trialData =
    (posixToMillis trialData.currentTime) >= (posixToMillis <| endTime trialData)


-- SUBSCRIPTIONS


subscriptions : AudioData -> ProgramState -> Sub Msg
subscriptions _ programState =
    Sub.batch
        [ rocketAnimationSub programState
        , onKeyPress keyDecoder
        , onResize (\w h -> HeightChanged h)
        ]

rocketAnimationSub : ProgramState -> Sub Msg
rocketAnimationSub programState =
    case programState of
        Running ({currentTrial} as state) ->
            case currentTrial of
                Launched trialData ->
                    if (not <| hasEnded trialData) then
                        onAnimationFrame StepRocket
                    else
                        Sub.none
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


view : AudioData -> ProgramState -> Html Msg
view _ programState =
    layout
        [ Font.family
            [ Font.typeface "Lato"
            , Font.sansSerif
            ]
        , Font.color lightgrey
        , Background.color background
        ]
        ( case programState of
            Initializing state ->
                viewWelcome state

            Running state ->
                row
                    [ height fill
                    , width fill
                    , inFront (menuPanel state)
                    ]
                    ( if trialIdx state <= nTrials state.sessionType then
                        viewExperiment state
                    else
                        viewSessionEnd state
                    )
            )

viewExperiment : ExperimentState -> List (Element Msg)
viewExperiment state =
    [ viewDebugLog state.debug state.debugLog
    , vertSeparator
    , column
        [ alignLeft
        , centerY
        , height fill
        , width <| fillPortion 6
        -- , explain Debug.todo
        ]
        [ drawScreen state
        ]
    , vertSeparator
    , column
        [ width fill
        , height fill
        ]
        [
        ]
    ]

viewSessionEnd : ExperimentState -> List (Element Msg)
viewSessionEnd state =
    [ column
        [ centerX
        , height fill
        --, explain Debug.todo
        ]
        [ heightFillerW 3
        , el
            [ centerX
            , centerY
            , Font.light
            , Font.size 50
            ]
            <| text <| "session complete"
        , heightFillerW 2
        , el
            [ centerX
            , Font.size 25
            ]
            <| text "total score"
        , el
            [ centerX
            , Font.bold
            , Font.size 40
            , padding 10
            ]
            <| text <| fromInt (state.totalScore)
        , row
            [ height <| fillPortion 3
            , centerX
            ]
            [ text "press Space to download your data"
            ]
        ]
    ]

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
        ]

viewDebugEntries : List String -> List (Element Msg)
viewDebugEntries log =
    case log of
        [] -> []

        msg::msgs ->
            text msg :: viewDebugEntries msgs


viewWelcome : SetupState -> Element Msg
viewWelcome state = 
    row
        [ width fill
        , height fill
        , inFront <| viewHelp state
        ]
        [ viewDebugLog state.debug state.debugLog
        , column
            [ centerX
            , centerY
            , height fill
            , width <| fillPortion 3
            , padding 75
            --, explain Debug.todo
            ]
            [ heightFillerW 3
            , el
                [ centerX
                , Font.light
                , Font.size 50
                ]
               <| text <| "the model uncertainty experiment"
            , heightFillerW 2
            , el
                [ centerX ]
                <| text "session type"
            , row
                [ centerX
                , alignBottom
                , spacing 30
                , padding 20
                ]
                [  el 
                    ( [ onClick (SetSessionType Train)
                      ]
                      ++ case state.sessionType of
                            Test ->
                                [ Font.light
                                ]
                            Train ->
                                [ Font.heavy
                                ]
                    )
                    <| text "Train"
                , el
                    [ height <| px 70
                    , Border.width 1
                    ]
                    none
                , el
                    ( [ onClick (SetSessionType Test)
                      ]
                      ++ case state.sessionType of
                            Test ->
                                [ Font.heavy
                                ]
                            Train ->
                                [ Font.light
                                ]
                    )
                    <| text "Test"
                ]
            , row
                [ height <| fillPortion 3
                , centerX
                , spacing 20
                ]
                [ text "press Space to start"
                ]
            ]
        , column [width fill] []
        ]

viewHelp : SetupState -> Element Msg
viewHelp state =
    let 
        radius = 12
    in
    if state.showHelp then
        row
            [ width fill
            , height fill
            , onClick ShowHelp
            , Background.color <| rgba 0 0 0 0.5
            ]
            [ el [width fill] none
            , el
                [ centerX
                , centerY
                , Background.color lightgrey
                , Font.color darkgrey
                , width fill
                , onClick NoOp
                , Border.rounded radius
                , padding radius
                ]
                <| text "I am a help text."
            , el [width fill] none
            ]
    else
        el
            [ alignBottom
            , alignRight
            , padding radius
            ]
            <| el
                [ padding <| radius
                , centerX
                , centerY
                , Border.rounded <| 5*radius
                , Background.color lightgrey
                , Font.color darkgrey
                , Font.size 36
                , onClick ShowHelp
                , Border.shadow
                    { offset = (3, 3)
                    , size = 3
                    , blur = 5
                    , color = rgb 0 0 0
                    }
                ]
                <| text "?"
            

drawScreen : ExperimentState -> Element Msg
drawScreen state = 
    el
        [ centerX
        , centerY
        , inFront
            <| el
                [ padding 12
                , alignTop
                , centerX
                ]
                <| text <| "score: " ++ (fromInt state.totalScore)
        ]
        <| html <| svg
            [ SA.width <| fromFloat state.width
            , SA.height <| fromFloat state.height
            , SA.viewBox
                <| String.join " "
                    ( List.map fromFloat
                        [ 0
                        , 0
                        , state.width
                        , state.height
                        ]
                    )
            , SA.fill "rgb(51,51,51)"
            -- , centerX
            ]
            <| Svg.rect
                [SA.width <| fromFloat state.width
                , SA.height <| fromFloat state.height
                ]
                [Svg.text "foo"]
            :: drawCue state
            ++ drawTarget state
            ++ drawRocket state
            ++ drawFeedback state


drawRocket : ExperimentState -> List (Svg Msg)
drawRocket ({currentTrial} as state) =
        case state.currentTrial of
            Launched trialData ->
                let
                    rocket =
                        { x = trialData.xFrac * state.width
                        , y = getRocketY trialData state
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
        case currentTrial of
            Launched trialData ->
                let
                    size = getFeedbackSize state
                in
                    if hasEnded trialData then
                        if (result trialData) then
                            Svg.polyline
                                [ SA.points <| pointsToString <|
                                    makeCheckmarkPolygonPoints (state.width/2, state.height/2) (getFeedbackSize state)
                                , SA.stroke "rgb(0,255,0)"
                                , SA.strokeWidth <| fromFloat <| (getFeedbackSize state) / 4
                                ]
                                []
                            :: Svg.text_
                                [ SA.x <| fromFloat (state.width/2 - size * 0.70)
                                , SA.y <| fromFloat (state.height/2 + 1.5 * size)
                                , SA.fontFamily "Lato"
                                , SA.fontSize <| fromFloat (size * 0.4)
                                , SA.fill "rgb(0,255,0)"
                                ]
                                [ Svg.text <| "+" ++ (fromInt <| (score << error) trialData)
                                ]
                            :: []
                        else
                            let
                                --err = fromInt <| abs <| round currentTrial.error
                                suffix =
                                   if error trialData < 0 then
                                       "late"
                                   else
                                       "early"
                            in
                            Svg.polyline
                                [ SA.points <| pointsToString <|
                                    makeCrossPolygonPoints (state.width/2, state.height/2) size
                                , SA.stroke "rgb(255,0,0)"
                                , SA.strokeWidth <| fromFloat <| size / 4
                                ]
                                []
                            :: Svg.text_
                                [ SA.x <| fromFloat (state.width/2 - size * 0.70)
                                , SA.y <| fromFloat (state.height/2 + 1.5 * size)
                                , SA.fontFamily "Lato"
                                , SA.fontSize <| fromFloat (size * 0.4)
                                , SA.fill "rgb(255,0,0)"
                                --, SA.fontStyle "italic"
                                ]
                                [ Svg.text <| "too " ++ suffix
                                --Svg.text <| err ++ " ms too " ++ suffix
                                ]
                            :: []
                    else
                        []

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
getX ({currentTrial} as state) =
    let 
        xFrac =
            case currentTrial of
                Idle data -> data.xFrac
                Waiting data -> data.xFrac
                Launchable data -> data.xFrac
                Launched data -> data.xFrac
    in
        xFrac * state.width


getCueX : ExperimentState -> Float
getCueX state =
    getX state - (getCueWidth state) / 2


getStartY : ExperimentState -> Float
getStartY state =
    (1 - params.startYFrac) * state.height


getTargetY : ExperimentState -> Float
getTargetY state =
    (1 - params.targetYFrac) * state.height


getRocketY : TrialData -> ExperimentState -> Float
getRocketY trialData state =
    let
        dt = toFloat
            <| (posixToMillis trialData.currentTime)
            - (posixToMillis trialData.launchTime)

        dy = dt / params.flightDuration

        flightDist = (params.targetYFrac - params.startYFrac)
    in
        state.height * (1 - (params.startYFrac + flightDist * dy))


getRocketRadius : ExperimentState -> Float
getRocketRadius state =
    params.rocketSizeFrac * state.height


getCueWidth : ExperimentState -> Float
getCueWidth state =
    params.cueWidthFrac * state.height


getCueHeight : ExperimentState -> Float
getCueHeight state =
    (getCueWidth state) / 5


getTargetSize : ExperimentState -> Float
getTargetSize state =
    params.targetSizeFrac * state.height


getFeedbackSize : ExperimentState -> Float
getFeedbackSize state =
    params.feedbackSizeFrac * state.height


heightFillerW weight =
    el
        [ height <| fillPortion weight
        ]
        none


background = rgb 0.2 0.2 0.2
grey = rgb 0.6 0.6 0.6
lightgrey = rgb 0.8 0.8 0.8
darkgrey = rgb 0.4 0.4 0.4

greytint : Float -> Color
greytint f =
    rgb f f f


vertSeparator : Element msg
vertSeparator =
    let
        sep : Color -> Element msg
        sep color =
            el
                [ height <| fillPortion 3
                , Border.widthEach
                    { bottom=0
                    , left=0
                    , right=1
                    , top=0
                    }
                , Border.color color
                ]
                none

        tints : List Float
        tints = [0.25, 0.3, 0.35, 0.4]

        greys : Float -> Float -> Int -> List Float
        greys start stop n =
            List.map
                ((+) start << (/) (toFloat n) << (*) (stop-start) << toFloat)
                (List.range 1 n)
            -- [0.8, 0.75, 0.7, 0.65, 0.6, 0.55, 0.5, 0.45, 0.4, 0.35, 0.3, 0.25]
    in
        column
            [ height fill
            ]
            ( [ el [height <| fillPortion 5] none
              ]
              ++ (List.map (sep << greytint) tints)
              ++ ((List.reverse << List.map (sep << greytint)) tints)
              -- ++ ((List.reverse << List.map (sep << greytint)) <| greys 0.2 0.8 20)
              ++ [ el [height <| fillPortion 5] none
              ]
            )


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
              , spacing <| round <| 0.5 * radius
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
                            , menuButton background [alignRight, alignTop ] MenuClose
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
                            { onPress = Just DownloadResults
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
                                    ++ " of " ++ (fromInt <| nTrials state.sessionType)
                                , text "##%"
                                ]
                            ]
                        ]
                    ]
            ]
            none
    else
        menuButton
            lightgrey
            [ alignTop
            , alignRight
            ]
            MenuOpen



-- AUDIO


renderAudio : AudioData -> ProgramState -> Audio
renderAudio _ programState = 
    case programState of
        Running state ->
            case state.beep of
                Just beepSound ->
                    case state.beepAt of
                        Nothing -> silence
                        Just time ->
                            audio beepSound time

                Nothing -> silence

        _ ->
            silence
