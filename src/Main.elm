port module Main exposing (..)

import Animator
import Animator.Inline

import Audio
    exposing
        ( Audio
        , AudioCmd
        , AudioData
        , audio
        , silence
        , elementWithAudio
        )

import Browser
import Browser.Events
    exposing
        ( onKeyPress
        , onKeyDown
        , onAnimationFrame
        , onResize
        )
import Browser.Dom
    exposing
        ( Viewport
        , getViewport
        )

import Char as C
import Color

import Element exposing (..)
import Element.Input
    exposing
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
import Element.Events
    exposing (onClick, onLoseFocus)
import Element.Font as Font

import File.Download as Download

import Html

import Json.Decode as Decode
import Json.Encode as Encode

import List as L

import Maybe as M

import Process

import Random

import Rocket

import String as S
    exposing
        ( fromInt
        , fromFloat
        )

import Svg exposing (Svg, svg)
import Svg.Attributes as SA

import Task
    exposing
        ( perform
        , andThen
        )

import TextElements as TE
    exposing
        ( TextList(..)
        , ListItem(..)
        )

import Time
    exposing
        ( Posix
        , now
        , millisToPosix
        , posixToMillis
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
        , Done "Show success percentage in side menu"
        , Done "Show trialIdx in side menu"
        , Do Low "reset experiment when reset button is clicked"
        ]
    , Do Low "Convert debugging flow into a monadic construction (instead of making separate debug calls)"
    , P "documentation"
        [ Do Medium "annotate utility functions"
        , Do High "annotate main program flow functions"
        , Do Medium "annotate Msg types"
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
            , Done "Animate (i.e. fade in/out) the 'press space to start' string"
            , P  "Show help"
                [ Done "Show help button on welcome screen"
                , Done "Replace help button in lower corner of screen with horizontal bar and downward arrow (i.e. to signal 'expandable') below the main title and expand the help section when it is clicked"
                , Done "Show popup when 'help' is clicked"
                , Done "Write help text in popup"
                ]
            , Do Low "Make a demo mode to explain the experiment"
            , Done "Make two buttons to allow choosing Train vs Test session"
            , Do Low "Before starting the experiment, show countdown timer with countdownBeep and goBeep"
            ]
    , P "Experiment logistics"
            [ P "Make sure the experiment has the correct no. trials"
                [ Done "On mixture sessions, start with 50 trials of 'baseline'"
                , Done "What is the model type during the baseline of mixture sessions?"
                , Done "The baseline is quadratic"
                , Done "On mixture sessions, have 100 trials per mixture condition"
                , Do High "Remove override of having only 5 trials on training session"
                ]
            , Done "Don't respond to multiple space presses simultaneously"
            , P "Sample random Profile for each trial"
                [ P "When updating with the MakeTrial message -> Sample random profile and send it to 'mkTrial"
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
        , Done "Make debugLog a floating object"
        ]
    ]



-- HELP


nHelpPages : Int
nHelpPages = L.length <| help 0

help : Float -> List (List (Element Msg), Element Msg)
help fontSize =
    let
        p = L.map (\str -> paragraph [] [ text str ]) << S.lines
    in
    [ ( p """Welcome!
    This is an experiment in the form of a game. It is designed to investigate the human ability to learn and predict intervals of time.
    """
-- The experiment consists of independent but similar trials.
    , none
    )
    , ( p """The goal is simple: You launch a rocket from the bottom of the screen, to hit a target at the top.
However, the target only appears after an unknown interval and, when it is invisible, it cannot be hit. Therefore, it is important to launch the rocket at the right time!
    """
    , none
    )
    , ( p "Each trial follows the same procedure:"
        ++
        ( TE.ol
            [ Font.size <| round <| 26 / 32 * fontSize
            ]
            [ Item "You initiate it by pressing the 'z' key."
            , Item "After a short while, a rectangular cue, at the bottom of the screen, indicates the start of the trial."
            , SubList "From then on:"
                <| Ol
                [ Item "You can launch the rocket, by pressing the Space bar."
                , Item "The target will appear at the top of the screen at some point."
                ]
            , Item "Repeat!"
            ]
        )
        , none
    )
    , ( p """Initially, you will not know when exactly to launch the rocket. Don't worry, this is part of the game!
        As you progress through the trials, you will acquire the skill of well-timed rocket launching.
    """
    , none
    )
    , ( p """Each trial ends as soon as the target appears.
        When this happens,"""
        ++ ( TE.ul
            [ Font.size <| round <| 26/32 * fontSize
            ]
            [ Item "The rocket stops moving."
            , Item "Points are awarded if it's close to the target."
            , Item "Otherwise, a hint shows whether the rocket was launched too late or too early."
            ]
        ) ++ p "It is your task to score as many points as possible before the game is over."
    , none
    )
    , ( p """The game comes in two versions, a 'Train' and a 'Test' one. Make sure to select the right type before you start.
    Good luck!
    """
    , none
    )
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
port audioPortToJS : Encode.Value -> Cmd msg
port audioPortFromJS : (Decode.Value -> msg) -> Sub msg



-- MODEL


type ProgramState
    = Fetching
        { maybeHeight : Maybe Float
        , maybeSeq : Maybe TrainSeq
        , debugLog : List String
        }
    | Initializing SetupState
    --| Registering SetupState (Maybe Char, Maybe Char, Maybe Char)
    | Registering SetupState (List Char) Bool
    | BuildState
        ExperimentData
        (Maybe Posix)
        (Maybe Profile)
        (Maybe Float)
    | Running RunState


type alias SetupState =
    { beep : Maybe Audio.Source
    , debug : Bool
    , debugLog : List String
    , height : Float
    , helpPage : Animator.Timeline Int
    , sessionType : SessionType
    , helpShown : Animator.Timeline Bool
    , textAlpha : Animator.Timeline ()
    }

type alias RunState = (ExperimentData, Trial)

type alias ExperimentData =
    { beep : Maybe Audio.Source
    , beepAt : Maybe Posix
    , debug : Bool
    , debugLog : List String
    , cue : Bool
    , height : Float
    , participantId : String
    , results : List TrialData
    , sessionType : SessionType
    , showMenu : Animator.Timeline Bool
    , target : Bool
    , textAlpha : Animator.Timeline ()
    , totalScore : Int
    , width : Float 
    }

buildState : ExperimentData -> ProgramState
buildState state = BuildState state Nothing Nothing Nothing

run : ProgramState -> ProgramState
run progState =
    case progState of
        BuildState data (Just time) (Just profile) (Just x) ->
            Running (data, mkTrial time profile x)

        _ -> progState

initState : Float -> TrainSeq -> List String -> ProgramState
initState h seq log =
    Initializing
        { debug = False
        , debugLog = log
        , sessionType = Train seq
        , height = h
        , helpShown = Animator.init False
        , helpPage = Animator.init 0
        , beep = Nothing
        , textAlpha = Animator.init ()
        }

init : flags -> (ProgramState, Cmd Msg, AudioCmd Msg)
init _ =
    ( Fetching
        { maybeHeight = Nothing
        , maybeSeq = Nothing
        , debugLog = []
        }
    , Cmd.batch
        [ Task.perform viewport2HeightChanged getViewport
        , sampleTrainSession
        ]
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

initExperiment : SetupState -> String -> ExperimentData
initExperiment state id =
    { height = 740
    , width = 960
    , results = []
    --, currentTrial = mkTrial time profile x
    , debugLog = "initiating experiment state"::state.debugLog
    , cue = False
    , target = False
    , debug = state.debug
    , showMenu = Animator.init False
    , sessionType = state.sessionType
    , totalScore = 0
    , beep = state.beep
    , beepAt = Nothing
    , textAlpha = state.textAlpha
    , participantId = id
    }

--nextTrial : ExperimentData -> Posix -> Profile -> Float -> ExperimentData
nextTrial : ProgramState -> ProgramState
nextTrial progState =
    case progState of
        BuildState state (Just mkTime) (Just profile) (Just x) ->
                Running (state, (mkTrial mkTime profile x))

        _ -> progState

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
    , cueDuration = 50 --35.0
    , targetDuration = 150.0
    , iti = 1500.0/2
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
        , makeTime : Posix
        }
    | Waiting
        { xFrac : Float
        , profile : Profile
        , makeTime : Posix
        , initialDelay : Float
        , initTime : Posix
        }
    | Launchable
        { xFrac : Float
        , profile : Profile
        , makeTime : Posix
        , initialDelay : Float
        , initTime : Posix
        , startTime : Posix
        , cueHideTime : Maybe Posix
        , measuredEndTime : Maybe Posix
        }
    | Launched TrialData


type alias TrialData =
    { xFrac : Float
    , profile : Profile
    , makeTime : Posix
    , initialDelay : Float
    , initTime : Posix
    , startTime : Posix
    , cueHideTime : Maybe Posix
    , currentTime : Posix
    , launchTime : Posix
    , measuredEndTime : Maybe Posix
    }


trialIdx : ExperimentData -> Int
trialIdx state =
    state.results |> L.length |> (+) 1


statusToString : Trial -> String
statusToString trial =
    case trial of
        Idle _ -> "idle"
        Waiting _ -> "waiting"
        Launchable _ -> "launchable"
        Launched _ -> "launched"
        

type SessionType
    = Train TrainSeq
    | Test TestSeq

type alias TrainSeq = (ModelID, ModelID, ModelID)
type alias TestSeq = (ModelID, (ModelID, ModelID, ModelID), (ModelID, ModelID, ModelID))


trainSeq : Int -> TrainSeq
trainSeq n = 
    case n of
        1 -> (C,L,Q)
        2 -> (C,Q,L)
        3 -> (L,C,Q)
        4 -> (L,Q,C)
        5 -> (Q,L,C)
        6 -> (Q,C,L)
        _ -> (Q,C,L)

testQSeq : TestSeq
testQSeq =
    (Q
    , (QC1, QC2, QC3)
    , (LC1, LC2, LC3)
    )

testLSeq : TestSeq
testLSeq = 
    (L
    , (LC3, LC2, LC1)
    , (QC3, QC2, QC1)
    )


sessionType2Str : SessionType -> String
sessionType2Str sess =
    case sess of
        Train (m1,m2,m3) ->
            "train" ++ S.concat (L.map modelId2Str [m1,m2,m3])

        Test (m0,(m1,m2,m3),(m4,m5,m6)) ->
            "test" ++ S.concat (L.map modelId2Str [m0,m1,m2,m3,m4,m5,m6])


nTrials : SessionType -> Int
nTrials s =
    case s of
        Train _ -> 360
        Test _ -> 650


sessionComplete : ExperimentData -> Bool
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

modelId2Str : ModelID -> String
modelId2Str id =
    case id of
        C -> "C"
        L -> "L"
        Q -> "Q"
        QC1 -> "QC1"
        QC2 -> "QC2"
        QC3 -> "QC3"
        LC1 -> "LC1"
        LC2 -> "LC2"
        LC3 -> "LC3"



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
        Train (t1,t2,t3) ->
            if iTr <= 120 then
                t1
            else if iTr <= 240 then
                t2
            else --if iTr <= 360 then
                t3
        Test (t1,(t2,t3,t4),(t5,t6,t7)) ->
            if iTr <= 50 then
                t1
            else if iTr <= 150 then
                t2
            else if iTr <= 250 then
                t3
            else if iTr <= 350 then
                t4
            else if iTr <= 450 then
                t5
            else if iTr <= 550 then
                t6
            else --if iTr <= 650 then
                t7
            

-- UPDATE


type Msg
    = ShowHelp Bool
    | SetHelpPage Int
    --| SetDebugging Bool
    | ToggleDebugging
    | Register
    | Blink
    | InputChar Char
    | Enter
    | BackSpace
    | HeightChanged Int
    | MenuToggle Bool
    | BackgroundClick
    | DownloadResults
    | DownloadDebugTrace
    | Debug String
    | NoOp
    | SoundLoaded (Result Audio.LoadError Audio.Source)
    | SetTestSession TestSeq
    | SampleTrainSession
    | SetTrainSession TrainSeq
    | Space
    | SlashOrZPressed
    | NextTrial Posix
    | TimeStamp Posix
    | RandomProfile Profile
    | RandomX Float
    | InitTrial Posix
    | RandomDelay Posix Float
    | StartTrial Posix
    | HideCue Posix
    | LaunchRocket Posix
    | StepRocket Posix
    | ShowTarget Posix
    | AnimationStep AnimationType Posix


type AnimationType
    = ToggleHelp
    | ChangeHelpPage
    | ToggleMenu
    | TextAlpha


logMsg : String -> { a | debugLog : List String } -> { a | debugLog : List String }
logMsg msg state =
    { state | debugLog = msg :: state.debugLog }

newTrialCmd : ExperimentData -> Cmd Msg
newTrialCmd state =
    Cmd.batch 
        [ Random.generate
            RandomProfile
            (getDistModel
                <| getModelId ((trialIdx state) + 1) state.sessionType
            )
        , Random.generate (RandomX) (Random.float 0 1)
        , Task.perform TimeStamp Time.now
        , Task.perform viewport2HeightChanged getViewport
        ]

saveTrial : RunState -> RunState
saveTrial ((state,trial) as runState) =
    case trial of
        Launched trialData ->
            (logMsg
                ("MakeTrial [trialIdx="++(fromInt <| trialIdx state)++"]")
                { state
                | target = False
                , results = state.results ++ [trialData]
                , totalScore = state.totalScore + (score << error) trialData
                }
            , trial
            )

        _ -> runState

update : AudioData -> Msg -> ProgramState -> (ProgramState, Cmd Msg, AudioCmd Msg)
update _ msg programState =
    let
        noCmd newState = (newState, Cmd.none, Audio.cmdNone)
        noOp = noCmd programState
    in
    case programState of
        Fetching state ->
            case (msg, state.maybeHeight, state.maybeSeq) of
                (HeightChanged h, _, Nothing) ->
                    ( Fetching
                        { state
                        | maybeHeight = Just (toFloat h)
                        , debugLog = "setting height" :: state.debugLog
                        }
                    , Cmd.none
                    , Audio.cmdNone
                    )

                (HeightChanged h, _, Just seq) ->
                    ( initState (toFloat h) seq state.debugLog
                    , Cmd.none
                    , Audio.cmdNone
                    )

                (SetTrainSession seq, Nothing, _) ->
                    ( Fetching
                        { state
                        | maybeSeq = Just seq
                        , debugLog = "setting training sequence" :: state.debugLog
                        }
                    , Cmd.none
                    , Audio.cmdNone
                    )

                (SetTrainSession seq, Just h, _) ->
                    ( initState h seq state.debugLog
                    , Cmd.none
                    , Audio.cmdNone
                    )

                (_,_,_) -> (programState, Cmd.none, Audio.cmdNone)

        Registering state idChars cursorVisible ->
            let
                updateState newS = Registering newS idChars cursorVisible
            in
            case msg of
                Blink ->
                    noCmd <| Registering state idChars (not cursorVisible)

                AnimationStep TextAlpha newAlpha ->
                    noCmd <| updateState <| Animator.update newAlpha textAlphaAnimator1 state

                InputChar char ->
                    noCmd <| Registering state (idChars ++ [char]) cursorVisible

                BackSpace ->
                    let
                        delChar : List Char -> List Char 
                        delChar = L.reverse << safeTail << L.reverse
--                            case cs of
--                                (a,b,Just c) -> (a,b,Nothing)
--                                (a,Just b,c) -> (a,Nothing,c)
--                                (Just a,b,c) -> (Nothing,b,c)
--                                _ -> cs
                    in
                        noCmd
                            <| Registering
                                (logMsg "Deleting char" state)
                                (delChar idChars)
                                cursorVisible

                Enter ->
                    if L.length idChars < 3
                    then noOp
                    else
                        let
                            id = (S.fromList idChars)
                            expData = initExperiment state id
                        in
                        ( buildState expData
                        , newTrialCmd expData
                        , Audio.cmdNone
                        )
--                    case idChars of
--                        (Just a, Just b, Just c) ->
--                            let
--                                id = S.concat <| L.map S.fromChar [a,b,c]
--                            in
--                            noCmd <|
--                                buildState (initExperiment state id) Nothing
--
--                        (_,_,_) -> noCmd programState

                ToggleDebugging ->
                    noCmd <| updateState { state | debug = not state.debug }

                Debug str ->
                    noCmd <| Registering (logMsg str state) idChars cursorVisible

                _ -> noCmd programState

        BuildState state time_ profile_ x_ ->
            case (msg, (time_, profile_, x_)) of
                (TimeStamp t, (Nothing, _, _)) ->
                    noCmd <| run <| BuildState state (Just t) profile_ x_

                (RandomProfile p, (_, Nothing, _)) ->
                    noCmd <| run <| BuildState state time_ (Just p) x_

                (RandomX x, (_, _, Nothing)) ->
                    noCmd <| run <| BuildState state time_ profile_ (Just x)

                _ -> noCmd programState

        Running ((state,currentTrial) as runState) ->
            case msg of
                NextTrial time ->
                    case currentTrial of
                        Launched trialData ->
                            ( buildState <| (\(a,b) -> a) <| saveTrial runState
                            , newTrialCmd state
                            , Audio.cmdNone
                            )


                        _ -> noCmd programState

                _ ->
                    (\(newState, cmd) ->
                        (Running newState, cmd, Audio.cmdNone)
                    ) <| updateRunningState msg runState
            --(\(newState, cmd) -> (Running newState, cmd, Audio.cmdNone)) <| testRunUpdate msg state

        Initializing state -> 
            case msg of
                Register ->
                    noCmd <| Registering state [] True

--                TimeAndRandomProfileAndX time profile x ->
--                    ( M.withDefault programState <|
--                        initExperiment state time profile x
--                    , Task.perform viewport2HeightChanged getViewport
--                    , Audio.cmdNone
--                    )

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
                    (\(newState, cmd) ->
                        (Initializing newState, cmd, Audio.cmdNone)) <| updateSetupState msg state

updateSetupState : Msg -> SetupState -> (SetupState, Cmd Msg)
updateSetupState msg state =
    case msg of
        ShowHelp bool ->
            ( { state | helpShown = Animator.go Animator.quickly bool state.helpShown }
            , Cmd.none
            )

        SetHelpPage i ->
            if i >= 0 && i < nHelpPages then
                ( { state
                    | helpPage = Animator.go Animator.quickly i state.helpPage
                    , debugLog = ("SetHelpPage ["++fromInt i++"]") :: state.debugLog
                  }
                , Cmd.none
                )
            else
                (state, Cmd.none)

        SetTestSession seq ->
            ( { state
                | sessionType = Test seq
                }
            , Cmd.none
            )

        SampleTrainSession ->
            ( state
            , sampleTrainSession
            )

        SetTrainSession seq ->
            ( { state
                | sessionType = Train seq
                }
            , Cmd.none
            )

--        NextTrial time ->
--            ( { state
--              | debugLog = "Proceeding with first trial"::state.debugLog
--              }
--            , Random.generate
--                (TimeAndRandomProfile time)
--                (getDistModel <| getModelId 1 state.sessionType)
--            )

--        TimeAndRandomProfile time profile ->
--            ( state
--            , Random.generate
--                (TimeAndRandomProfileAndX time profile)
--                (Random.float 0 1)
--            )

        Space ->
            let
                helpPage = Animator.current state.helpPage
                helpShown = Animator.current state.helpShown

                maybeNext : Maybe Int
                maybeNext =
                    if (helpPage + 1) >= nHelpPages
                    then Nothing
                    else Just (helpPage + 1)
            in
                case (helpShown, maybeNext) of
                    (False, Nothing) ->
                        ( state
                        , Task.perform (\_ -> Register) Time.now
                        )

                    (True, Just n) ->
                        ( state
                        , Task.perform (\_ -> SetHelpPage <| helpPage + 1) Time.now
                        )

                    _ ->
                        ( state
                        , Task.perform (\_ -> ShowHelp (not helpShown)) Time.now
                        )

        ToggleDebugging ->
            ( { state | debug = not state.debug }
            , Cmd.none
            )

        Debug str ->
            ( { state | debugLog = str::state.debugLog }
            , Cmd.none
            )

        AnimationStep animation newTime ->
            case animation of
                ChangeHelpPage ->
                    ( Animator.update newTime helpPageAnimator state
                    , Cmd.none
                    )

                ToggleHelp ->
                    ( Animator.update newTime helpVisibilityAnimator state
                    , Cmd.none
                    )

                TextAlpha ->
                    ( Animator.update newTime textAlphaAnimator1 state
                    , Cmd.none
                    )

                _ -> (state, Cmd.none)

        _ ->
            (state, Cmd.none)


sampleTrainSession = Random.generate (SetTrainSession) (Random.map trainSeq (Random.int 1 6))

--testRunUpdate : Msg -> ExperimentData -> (ExperimentData, Cmd Msg)
--testRunUpdate msg ({currentTrial} as state) =
--    case msg of
--        Space ->
--            if sessionComplete state then
--                ( state
--                , Task.perform (\_ -> DownloadResults) Time.now
--                )
--            else
--                case currentTrial of
--                    Idle oldData ->
--                        let 
--                            newData =
--                                { xFrac = oldData.xFrac
--                                , profile = oldData.profile
--                                , makeTime = oldData.makeTime
--                                , initialDelay = 0
--                                , initTime = oldData.makeTime
--                                , startTime = oldData.makeTime
--                                , cueHideTime = Just oldData.makeTime
--                                , currentTime = oldData.makeTime
--                                , launchTime = oldData.makeTime
--                                , measuredEndTime = Just oldData.makeTime
--                                }
--                        in
--                            ( { state
--                              | currentTrial = Launched newData
--                              , debugLog = "Artificially completing trial"::state.debugLog
--                              }
--                            , Task.perform NextTrial Time.now
--                            )
--
--                    _ -> (state, Cmd.none)
--
--
--        NextTrial time ->
--            case currentTrial of
--                Launched trialData ->
--                    ( { state
--                      | target = False
--                      , results = state.results ++ [trialData]
--                      , totalScore = state.totalScore + (score << error) trialData
--                      , debugLog =
--                            ("MakeTrial [trialIdx="++(fromInt <| trialIdx state)++"]")::state.debugLog
--                      }
--                    , if sessionComplete state
--                    then Cmd.none
--                    else
--                      Random.generate
--                        (TimeAndRandomProfile time)
--                        (getDistModel <| getModelId ((trialIdx state) + 1) state.sessionType)
--                    )
--
--                _ -> ({state|debugLog = "ignoring 'NextTrial'"::state.debugLog}, Cmd.none)
--
--        TimeAndRandomProfile time profile ->
--            ( { state
--              | debugLog = 
--                    ("RandomProfile ["++profile2Str profile++"]")::state.debugLog
--              }
--            , Random.generate
--                (TimeAndRandomProfileAndX time profile)
--                (Random.float 0 1)
--            )
--
--        TimeAndRandomProfileAndX time profile x ->
--            let 
--                newData =
--                    { xFrac = x
--                    , profile = profile
--                    , makeTime = time
--                    , initialDelay = 0
--                    , initTime = time
--                    , startTime = time
--                    , cueHideTime = Just time
--                    , currentTime = time
--                    , launchTime = time
--                    , measuredEndTime = Just time
--                    }
--            in
--                ( { state
--                  | currentTrial = Launched newData
--                  , debugLog = ("sampleX [x="++fromFloat x++"]")
--                            ::"Artificially completing trial"
--                            ::state.debugLog
--                  }
--                , Task.perform NextTrial (Process.sleep 0.1 |> andThen (\_ -> Time.now))
--                )
--
--        DownloadDebugTrace ->
--            ( state
--            , Download.string
--                "modelUncertainty_debugging"
--                "text/plain"
--                (unlines state.debugLog)
--            )
--                
--
--        DownloadResults ->
--            ( state
--            , downloadResults state
--            )
--
--        _ ->
--            ( state, Cmd.none )
        

updateRunningState : Msg -> RunState -> (RunState, Cmd Msg)
updateRunningState msg ((state,currentTrial) as runState) =
    let
        updateState : ExperimentData -> RunState
        updateState newState = (newState,currentTrial)

        update_ : ExperimentData -> Trial -> RunState
        update_ newState newTrial = (newState, newTrial)

        noOp = (runState, Cmd.none)
    in
    case msg of
        Debug str ->
            ( updateState <| logMsg str state
            , Cmd.none
            )


--        TimeAndRandomProfile time profile ->
--            ( { state
--              | debugLog = 
--                    ("RandomProfile ["++profile2Str profile++"]")::state.debugLog
--              }
--            , Random.generate
--                (TimeAndRandomProfileAndX time profile)
--                (Random.float 0 1)
--            )

--        TimeAndRandomProfileAndX time profile x ->
--            ( { state
--              | currentTrial = mkTrial time profile x
--              , debugLog = ("sampleX [x="++fromFloat x++"]")::state.debugLog
--              }
--            , Cmd.none
--            )

        Space ->
            if sessionComplete state then
                ( runState
                , Task.perform (\_ -> DownloadResults) Time.now
                )
            else
                case currentTrial of
                    Launchable _ ->
                        ( update_ (logMsg "Space" state) currentTrial
                        , Task.perform LaunchRocket Time.now
                        )

                    _ -> noOp

        SlashOrZPressed ->
            case currentTrial of
                Idle _ ->
                    ( update_ (logMsg "SlashOrZPressed" state) currentTrial
                    , Task.perform InitTrial Time.now
                    )

                _ -> noOp

        InitTrial time ->
            case currentTrial of
                Idle _ ->
                        ( runState
                        , Random.generate (RandomDelay time) (Random.float 150.0 1000.0)
                        )

                _ -> noOp


        RandomDelay time delay ->
            case currentTrial of
                Idle oldData ->
                    let
                        newData =
                            { xFrac = oldData.xFrac
                            , profile = oldData.profile
                            , makeTime = oldData.makeTime
                            , initialDelay = delay
                            , initTime = time
                            }
                    in
                        ( update_
                              (logMsg ("RandomDelay [" ++ (fromFloat delay) ++ "] ") state)
                              (Waiting newData)
                        , Task.perform (StartTrial) (Process.sleep delay |> andThen (\_ -> now))
                        )

                _ -> noOp

        StartTrial startTime ->
            let
                durationStr = fromFloat <| duration currentTrial
                logStr =
                    ( "StartTrial [time="
                    ++ (fromInt <| posixToMillis startTime)
                    ++ ", duration="++durationStr++"]"
                    )

            in
                ( update_
                    (logMsg logStr { state | cue = True })
                    (startTrial startTime currentTrial)
                , Task.perform
                    HideCue
                    ( Process.sleep params.cueDuration
                        |> andThen (\_ -> now)
                    )
                )

        HideCue time ->
            let
                cmd = Task.perform
                        ShowTarget
                        (Process.sleep
                            ( (duration currentTrial)
                              - params.cueDuration
        --                      - params.targetDuration / 2
                            )
                            |> andThen (\_ -> now)
                        )

                logStr = "HideCue [time=" ++ (fromInt <| posixToMillis time) ++ "]"
                    
            in
                --case hideCue runState of
                case currentTrial of
                    Launched oldData ->
                        ( update_
                            (logMsg logStr { state | cue = False })
                            (Launched { oldData | cueHideTime = Just time })
                        , cmd
                        )

                    Launchable oldData -> 
                        ( update_
                            (logMsg logStr { state | cue = False })
                            (Launchable { oldData | cueHideTime = Just time })
                        , cmd
                        )
        
                    
                    _ -> noOp

        ShowTarget targetTime ->
            let
                updatedTrial = setTargetVisibleStamp targetTime currentTrial
            in
                case currentTrial of
                    Launched trialData ->
                        ( update_
                              (logMsg "ShowTarget"
                                   { state
                                   | target = True
                                   , beepAt = 
                                       if result trialData then
                                           Just targetTime
                                       else
                                           Nothing
                                   }
                              )
                              updatedTrial
                        , Task.perform
                            NextTrial
                            ( Process.sleep (params.targetDuration + params.iti)
                            |> andThen (\_ -> now)
                            )
                        )

                    Launchable _ ->
                        ( update_
                            (logMsg "ShowTarget" { state | target = True })
                            updatedTrial
                        , Cmd.none
                        )

                    _ -> noOp

        LaunchRocket time ->
            ( update_
                (logMsg "LaunchRocket" state)
                (launchTrial time currentTrial)
            , if state.target then --target is already visible -> start new trial
                Task.perform
                    NextTrial
                    (Process.sleep (params.targetDuration + params.iti)
                    |> andThen (\_ -> now)
                    )
            else
                 Cmd.none
            )

        StepRocket newTime ->
            case currentTrial of
                Launched trialData ->
                    let
                        newData =
                            if (posixToMillis newTime)
                            > (posixToMillis <| computeEndTime trialData) then
                                { trialData | currentTime = computeEndTime trialData }
                            else
                                { trialData | currentTime = newTime }
                    in
                        ( update_
                            { state
                            | debugLog =
                                ("StepRocket [time="++(fromInt <| posixToMillis newTime)++"]")::state.debugLog
                            }
                            (Launched newData)
                        , Cmd.none
                        )

                _ -> noOp

        HeightChanged h ->
            let
                newWidth = (toFloat h) * params.aspectRatio
                newHeight = toFloat h
            in
            ( update_
                { state
                | width = newWidth
                , height = newHeight
                , debugLog =  ("HeightChanged [h="++fromInt h++"]")::state.debugLog
                }
                currentTrial
            , Cmd.none
            )

        ToggleDebugging ->
            ( update_
                { state | debug = not state.debug }
                currentTrial
            , Cmd.none
            )

        MenuToggle bool ->
            ( update_
                { state | showMenu = Animator.go Animator.veryQuickly bool state.showMenu }
                currentTrial
            , Cmd.none
            )

        BackgroundClick ->
            ( update_
                { state | showMenu = Animator.go Animator.veryQuickly False state.showMenu }
                currentTrial
            , Cmd.none
            )

        DownloadDebugTrace ->
            ( runState
            , Download.string
                "modelUncertainty_debugging"
                "text/plain"
                (unlines state.debugLog)
            )
                

        DownloadResults ->
            ( runState
            , downloadResults state
            )

        AnimationStep aType newTime ->
            case aType of
                ToggleMenu ->
                    ( updateState <| Animator.update newTime experimentAnimator state
                    , Cmd.none
                    )
                    
                TextAlpha ->
                    ( updateState <| Animator.update newTime textAlphaAnimator2 state
                    , Cmd.none
                    )

                _ -> noOp

        _ -> noOp


downloadResults : ExperimentData -> Cmd msg
downloadResults state =
    Download.string
        ("modelUncertainty_session="
        ++ sessionType2Str state.sessionType
        ++ "_id="
        ++ state.participantId
        )
        "text/csv"
        (trialData2Csv state.results)


trialData2Csv : List TrialData -> String
trialData2Csv  =
    let
        trial2Csv trial =
            let
                startMillis = posixToMillis trial.startTime
            in
                S.join ","
                    [ fromFloat trial.xFrac
                    , fromFloat <| duration (Launched trial)
                    , fromInt
                        <| (posixToMillis trial.launchTime)
                        + (round params.flightDuration)
                        - startMillis
                    , fromInt <| posixToMillis trial.makeTime
                    , fromInt <| posixToMillis trial.initTime
                    , fromInt <| posixToMillis trial.startTime
                    , M.withDefault
                        "NaN"
                        ( M.map
                            (fromInt << posixToMillis) trial.cueHideTime
                        )
                    , fromInt <| posixToMillis trial.launchTime
                    , M.withDefault
                        "NaN"
                        ( M.map
                            (fromInt << posixToMillis)
                            --(fromInt << (\eT -> eT - startMillis) << posixToMillis)
                            trial.measuredEndTime
                        )
                    ]
    in
        S.join "\u{000A}" << L.map trial2Csv 


mkTrial : Posix -> Profile -> Float -> Trial
mkTrial mkTime profile x =
    Idle
        { xFrac = x
        , profile = profile
        , makeTime = mkTime
        }


startTrial : Posix -> Trial -> Trial
startTrial time trial =
    case trial of
        Waiting oldData ->
            let
                newData = 
                    { xFrac = oldData.xFrac
                    , profile = oldData.profile
                    , makeTime = oldData.makeTime
                    , initTime = oldData.initTime
                    , initialDelay = oldData.initialDelay
                    , startTime = time
                    , measuredEndTime = Nothing
                    , cueHideTime = Nothing
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
                    , makeTime = oldData.makeTime
                    , initTime = oldData.initTime
                    , initialDelay = oldData.initialDelay
                    , startTime = oldData.startTime
                    , currentTime = time
                    , launchTime = time
                    , measuredEndTime = oldData.measuredEndTime
                    , cueHideTime = oldData.cueHideTime
                    }
            in
                Launched newData

        _ -> trial

setTargetVisibleStamp : Posix -> Trial -> Trial
setTargetVisibleStamp time trial =
    case trial of
        Launchable oldData ->
            let
                newData = 
                    { xFrac = oldData.xFrac
                    , profile = oldData.profile
                    , makeTime = oldData.makeTime
                    , initTime = oldData.initTime
                    , initialDelay = oldData.initialDelay
                    , startTime = oldData.startTime
                    , measuredEndTime = Just time
                    , cueHideTime = oldData.cueHideTime
                    }
            in
                Launchable newData

        Launched oldData ->
            let
                newData = 
                    { xFrac = oldData.xFrac
                    , profile = oldData.profile
                    , makeTime = oldData.makeTime
                    , initTime = oldData.initTime
                    , initialDelay = oldData.initialDelay
                    , startTime = oldData.startTime
                    , currentTime = oldData.currentTime
                    , launchTime = oldData.launchTime
                    , measuredEndTime = Just time
                    , cueHideTime = oldData.cueHideTime
                    }
            in
                Launched newData

        _ -> trial


error : TrialData -> Int
error trialData =
    (posixToMillis <| computeEndTime trialData) - (posixToMillis <| arrivalTime trialData)


result : TrialData -> Bool
result trialData =
    (abs <| error trialData) <= (round <| params.targetDuration / 2)


successRate : List TrialData -> Float
successRate results =
    let
        boolToFloat b = if b then 1 else 0
        
        n = toFloat <| L.length results
    in
        (L.sum <| L.map (boolToFloat << result) results) / n


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

        Quadratic -> -4960 * xFrac^2 + 4960 * xFrac + 560


computeEndTime : TrialData -> Posix
computeEndTime trialData =
    millisToPosix
        <| (round <| duration (Launched trialData)) + (posixToMillis trialData.startTime)


hasEnded : TrialData -> Bool
hasEnded trialData =
    (posixToMillis trialData.currentTime) >= (posixToMillis <| computeEndTime trialData)


-- SUBSCRIPTIONS


subscriptions : AudioData -> ProgramState -> Sub Msg
subscriptions _ programState =
    Sub.batch
        <|
        [ rocketAnimationSub programState
        , onKeyPress <| keyDecoderWith actionKeyFilter
        , onResize (\w h -> HeightChanged h)
        ]
        ++ (case programState of
                Running (state,_) ->
                    [ Animator.toSubscription (AnimationStep ToggleMenu) state experimentAnimator
                    , Animator.toSubscription (AnimationStep TextAlpha) state textAlphaAnimator2
                    ]
                        
                Initializing state ->
                    [ Animator.toSubscription (AnimationStep ToggleHelp) state helpVisibilityAnimator
                    , Animator.toSubscription (AnimationStep ChangeHelpPage) state helpPageAnimator
                    , Animator.toSubscription (AnimationStep TextAlpha) state textAlphaAnimator1
                    ]
                
                Registering state _ _ ->
                    [ onKeyPress idCharDecoder
                    , onKeyDown <| keyDecoderWith backSpaceFilter
                    , Animator.toSubscription (AnimationStep TextAlpha) state textAlphaAnimator1
                    , Time.every 200 (\_ -> Blink)
                    ]

                BuildState _ _ _ _ -> [Sub.none]

                Fetching _ -> [Sub.none]
            )

toggleSub : Bool -> Sub msg -> Sub msg
toggleSub on sub = if on then sub else Sub.none

rocketAnimationSub : ProgramState -> Sub Msg
rocketAnimationSub programState =
    case programState of
        Running (_,currentTrial) ->
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


keyDecoder : Decode.Decoder String
keyDecoder = Decode.field "key" Decode.string

keyDecoderWith : (String -> Msg) -> Decode.Decoder Msg
keyDecoderWith filter = Decode.map filter <| keyDecoder

backSpaceFilter : String -> Msg
backSpaceFilter s =
    if s == "Backspace"
    then BackSpace
    else NoOp

idCharDecoder : Decode.Decoder Msg
idCharDecoder =
    let
        filter : String -> Msg
        filter s =
            if S.length s == 1 then
                filterAlpha s
            else if s == "Enter" then
                Enter
            else if s == " " then
                Space
            else if s == "backspace" then
                BackSpace
            else
                Debug s

        filterAlpha : String -> Msg
        filterAlpha s =
            M.withDefault (Debug s)
                <| M.map (InputChar << C.toUpper) -- lift the Msg constructor and the toUpper conversion inside the Maybe value
                <| L.head -- take the head of the list or Nothing
                <| L.filter C.isAlpha -- drop nonAlpha chars
                <| S.toList -- convert to list of chars
                <| S.left 1 s -- take leftmost char in string
    in
        Decode.map filter <| Decode.field "key" Decode.string
            
--keyDecoder : Decode.Decoder Msg
--keyDecoder =
--    Decode.map filterKey <| Decode.field "key" Decode.string


actionKeyFilter : String -> Msg
actionKeyFilter str =
    case str of
        " " ->
            Space

        "z" ->
            SlashOrZPressed

        "/" ->
            SlashOrZPressed

        _ ->
            NoOp



-- VIEW


view : AudioData -> ProgramState -> Html.Html Msg
view _ programState =
    layout
        [ Font.family
            [ Font.external
                { name = "Lato"
                , url = "https://fonts.googleapis.com/css?family=Lato:Thin,Light,Regular,Bold,Black"
                }
            --, Font.typeface "Lato"
            , Font.sansSerif
            ]
        , Font.color lightgray
        , Background.color background
        , clip
        , inFront 
            <| viewDebugLog programState
        ]
        ( case programState of
            Initializing state ->
                viewWelcome state

            Running ((state,currentTrial) as runState) ->
                row
                    [ height fill
                    , width fill
                    , inFront (menuPanel state.showMenu (Just state))
                    ]
                    ( if trialIdx state <= nTrials state.sessionType then
                        viewExperiment runState
                    else
                        viewSessionEnd state
                    )

            Registering state idChars cursorVisible -> viewRegistering state idChars cursorVisible

            BuildState _ _ _ _ -> Element.none

            Fetching _ -> Element.none
            )

viewExperiment : RunState -> List (Element Msg)
viewExperiment state =
    [ el [width fill] none
    , vertSeparator
    , column
        [ height fill
        , width <| fillPortion 6
        -- , explain Debug.todo
        ]
        [ viewScreen state
        ]
    , vertSeparator
    , el [width fill] none
    ]

viewSessionEnd : ExperimentData -> List (Element Msg)
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
            , alpha <|
                Animator.move
                    state.textAlpha
                    (\_ -> Animator.loop
                        (Animator.millis 2200)
                        (Animator.wave 0.3 1)
                    )
            ]
            [ text "press Space to download your data"
            ]
        ]
    ]

viewDebugLog : ProgramState -> Element Msg
viewDebugLog programState =
    let
        (doDebug, log) = 
            ( case programState of
                Initializing state -> (state.debug, state.debugLog)
                Running (state,_) -> (state.debug, state.debugLog)
                Fetching state -> (True, state.debugLog)
                Registering state _ _ -> (state.debug,state.debugLog)
                BuildState state _ _ _ -> (state.debug, state.debugLog)
            )
    in
        if doDebug then
            column
                [ scrollbarY
                , scrollbarX
                , alignLeft
                , centerY
                , height <| px 900
                , width <| px 300
                , padding 10
                ] <| viewDebugEntries log
        else
            none

viewDebugEntries : List String -> List (Element Msg)
viewDebugEntries log =
    case log of
        [] -> []

        msg::msgs ->
            text msg :: viewDebugEntries msgs


viewRegistering : SetupState -> List Char -> Bool -> Element msg
viewRegistering state idChars cursorVisible =
    let
        --mC2String : Maybe Char -> String
        --mC2String mC = M.withDefault "_" (M.map S.fromChar mC)

        --viewChar : Maybe Char -> Element msg
        viewChar : Char -> Element msg
        viewChar c =
            el
                [ Font.medium
                , Font.size 64
                ]
                <| text <| S.fromChar c
                --<| text <| mC2String mC

        viewCursor : Bool -> Element msg
        viewCursor b =
            let alpha_ = if b then 1 else 0
            in el [ alpha alpha_ ] <| viewChar '_'

        viewId : List Char -> Element msg
        viewId chars =
            row
                [ spacing 16
                , height <| px 64
                ]
                <| (L.map viewChar chars) ++ [viewCursor cursorVisible]

        confirmationMsg : Bool -> Element msg
        confirmationMsg b =
            if b
            then none
            else
                el
                    [ centerX
                    , centerY
                    , alpha <|
                        Animator.move
                            state.textAlpha
                            (\_ -> Animator.loop
                                (Animator.millis 2200)
                                (Animator.wave 0.3 1)
                            )
                    ]
                    <| text "press Enter to confirm"
    in
    column
        [ centerX
        , centerY
        , height fill
        ]
        [ el [ height <| fillPortion 3 ] none
        , el
            [ Font.light
            , Font.size 48
            , centerX
            ] <| text "type your identifier"
        , el [ height <| fillPortion 1 ] none
        , el
            [ centerX
            ]
            <| viewId idChars
        , el
            [ height <| fillPortion 3
            , width fill
            ]
            <| confirmationMsg (L.length idChars < 3)
        ]


viewWelcome : SetupState -> Element Msg
viewWelcome state = 
    let
        helpShown = Animator.current state.helpShown
        helpPage = Animator.current state.helpPage

        modifier = 
            Animator.linear state.helpShown <|
                \b ->
                    if b then
                        Animator.at 0
                    else
                        Animator.at 1

        animatedSize = (px << round << (*) modifier)
    in
    row
        [ width fill
        , height fill
        , clipY
        , clipX
        --, explain Debug.todo
        ]
        [ el [width fill] none
        , column
            [ centerX
            , centerY
            , height fill
            , width <| px 1211 --<| fillPortion 3
            --, padding 75
            , clipY
            --, explain Debug.todo
            ]
            [ el [height (fill |> maximum 75)] none
            , heightFillerW 3
            , el
                [ centerX
                , Font.light
                , Font.size 64
                , padding 32
                ]
               <| text <| "the model uncertainty experiment"
            , viewHelp state
            , el
                [ height <| fillPortion 2
                ]
                none
            , column
                [ centerX
                , Font.size 32
                , clipY
                , height <| animatedSize 142
                , alpha modifier
                ]
                [ el [centerX] <| text "session type"
                , row
                    [ centerX
                    , alignBottom
                    , spacing 30
                    , padding 20
                    ]
                    [  el 
                        ( [ onClick SampleTrainSession
                          ]
                          ++ case state.sessionType of
                                Test _ ->
                                    [ Font.light
                                    ]
                                Train _ ->
                                    [ Font.heavy
                                    ]
                        )
                        <| text "Train"
                    , el
                        [ height <| px 70
                        , Border.widthEach {top = 0, left = 1, right = 0, bottom = 0}
                        ]
                        none
                    , el
                        ( [ onClick (SetTestSession testQSeq)
                          ]
                          ++ case state.sessionType of
                                Test seq ->
                                    if fst seq == Q
                                    then
                                        [ Font.heavy
                                        ]
                                    else
                                        [ Font.light
                                        ]   
                                Train _ ->
                                    [ Font.light
                                    ]
                        )
                        <| text "TestQ"
                    , el
                        [ height <| px 70
                        , Border.widthEach {top = 0, left = 1, right = 0, bottom = 0}
                        ]
                        none
                    , el
                        ( [ onClick (SetTestSession testLSeq)
                          ]
                          ++ case state.sessionType of
                                Test seq ->
                                    if fst seq == L
                                    then
                                        [ Font.heavy
                                        ]
                                    else
                                        [ Font.light
                                        ]
                                Train _ ->
                                    [ Font.light
                                    ]
                        )
                        <| text "TestL"
                    ]
                ]
            , row
                [ height <| fillPortion 3
                , clipY
                , centerX
                , height
                    ( fillPortion 3
                        |> maximum (28 + (round <| 160 * modifier * state.height / 1080))
                        |> minimum (2*20)
                    )
                , alpha <|
                    Animator.move
                        state.textAlpha
                        (\_ -> Animator.loop
                            (Animator.millis 2200)
                            (Animator.wave 0.3 1)
                        )
                , onClick Space
                ]
                [ text <|
                    if (not helpShown) && (helpPage + 1) >= nHelpPages then
                        "press Space to start"
                    else
                        "press Space"
                    
                ]
                , el [height (fillPortion 3 |> maximum 75)] none
            ]
        , el [width fill] none
        ]

viewHelp : SetupState -> Element Msg
viewHelp state =
    let 

        helpShown = Animator.current state.helpShown
        helpPage = Animator.current state.helpPage

        radius = 12
        textWidth = 750
        textPadding = 30
        helpWidth = textWidth + 2*textPadding
        pageHeightMax = 550

        bold = (el [Font.medium])
        textbf = bold << text
        red =  (el [Font.color <| rgb 1 0 0])
        green = (el [Font.color <| rgb 0 1 0])
        blue =  (el [Font.color <| rgb 0 0 1])
        cyan =  (el [Font.color <| rgb 0 1 1])

        modifier = 
            Animator.linear state.helpShown <|
                \b ->
                    if b then
                        Animator.at 1
                    else
                        Animator.at 0

        lateModifier =
            Animator.linear state.helpShown <|
                \b ->
                    if b then
                        Animator.at 1
                    else
                        Animator.at 0
                            |> Animator.leaveLate 1


        viewHelpPage : Int -> Element Msg
        viewHelpPage i =
            if i >= nHelpPages then
                none
            else
                let
                    fontMin = 8
                    fontMax = 32
                    fontSize = fontMin + (fontMax - fontMin) * pageModifier

                    (helpText, helpGraphics) =
                        M.withDefault ([], none) <| getListEntry i (help fontSize)

                    pageModifier =
                        Animator.linear state.helpPage <|
                            \pageNum ->
                                if pageNum == i then
                                    Animator.at 1
                                else
                                    Animator.at 0
                in
                    textColumn
                        [ paddingXY 0 5
                        , Font.light
                        , Font.color white
                        , Font.size <| round <| fontMin + (fontMax - fontMin) * pageModifier
                        , spacing <| round <| fontSize / 2
                        , width <| px <| round <| pageModifier * (textWidth - 130)
                        , alpha pageModifier
                        ]
                        helpText
    in
        column
            [ centerX
            --, explain Debug.todo
            --, Background.color lightgray
            --, Font.color darkgray
           -- , Font.family
           --     [ Font.typeface "open-sans"
           --     , Font.sansSerif
           --     ]
            --, Font.light
            , width <| px helpWidth
            --, Border.rounded radius
            ]
            [ row
                [ centerY
                , height
                    ( fill
                        |> maximum (round <| modifier * pageHeightMax)
                    )
                , alpha lateModifier
                ]
                [ (if helpPage > 0 then
                    el
                        [ onClick <| SetHelpPage (helpPage - 1)
                        , width <| px 50
                        ]
                        <| el [centerX] <| arrowHeadLeft 45
                else
                    el [width <| px 50] none
                )
                , row
                    [ width fill
                    , centerX
                    , clip
                    , centerY
                    --, height <| px <| round <| modifier * pageHeightMax
                    , paddingXY textPadding 0
                    ]
                    <| L.map viewHelpPage <| L.range 0 nHelpPages
                , (if helpPage + 1 < nHelpPages then
                    el
                        [ onClick <| SetHelpPage (helpPage + 1)
                        , width <| px 50
                        ]
                        <| el [centerX] <| arrowHeadRight 45
                else
                    el [width <| px 50] none
                )
                ]
            , el
                [ centerX
                , alignBottom
                , paddingEach
                    { top = round <| 25 * modifier
                    , left = 0
                    , right = 0
                    , bottom = round <| 13 * modifier
                    }
                , onClick <| ShowHelp False
                , height <| px <| round <| 85 * modifier
                , alpha modifier
                ]
                <| arrowHeadsUp <| 75 * modifier
            , horzSeparator
            , el
                [ centerX
                , alignTop
                , onClick <| ShowHelp True
                , padding <| round <| 13 * (1-modifier)
                , height <| px <| round <| 60 * (1-modifier)
                , alpha (1 - modifier)
                --, explain Debug.todo
                ]
                <| arrowHeadsDown <| 75 * (1 - modifier)
            ]


arrowHeadLeft : Float -> Element Msg
arrowHeadLeft h =
    let
        w = h/5

        linePoints : List (Float,Float)
        linePoints = [(w,0), (1,h/2), (w,h)]

        arrowHead : Float -> Svg Msg
        arrowHead offsetX =
            Svg.polyline
                [ SA.points <| pointsToString (L.map (\(x,y) -> (x+offsetX,y)) linePoints)
                , SA.stroke <| svgGray grays.lightgray
                , SA.strokeWidth <| fromFloat <| 2
                , SA.fillOpacity "0"
                ]
                []
    in
    html <| svg
        [ SA.width <| fromFloat w
        , SA.height <| fromFloat h
        ]
        [ arrowHead 0
        ]


arrowHeadRight : Float -> Element Msg
arrowHeadRight h =
    let
        w = h/5

        linePoints : List (Float,Float)
        linePoints = [(1,0), (w,h/2), (1,h)]

        arrowHead : Float -> Svg Msg
        arrowHead offsetX =
            Svg.polyline
                [ SA.points <| pointsToString (L.map (\(x,y) -> (x+offsetX,y)) linePoints)
                , SA.stroke <| svgGray grays.lightgray
                , SA.strokeWidth <| fromFloat <| 2
                , SA.fillOpacity "0"
                ]
                []
    in
    html <| svg
        [ SA.width <| fromFloat w
        , SA.height <| fromFloat h
        ]
        [ arrowHead 0
        ]


arrowHeadsDown : Float -> Element Msg
arrowHeadsDown w =
    let
        h = w/5

        linePoints : List (Float,Float)
        linePoints = [(0,1), (w/2,h), (w,1)]

        arrowHead : Float -> Svg Msg
        arrowHead offsetY =
            Svg.polyline
                [ SA.points <| pointsToString (L.map (\(x,y) -> (x,y + offsetY)) linePoints)
                , SA.stroke <| svgGray grays.lightgray
                , SA.strokeWidth <| fromFloat <| 2
                , SA.fillOpacity "0"
                ]
                []
    in
    html <| svg
        [ SA.width <| fromFloat w
        , SA.height <| fromFloat (2*h+1)
        ]
        [ arrowHead 0
        , arrowHead h
        ]


arrowHeadsUp : Float -> Element Msg
arrowHeadsUp w =
    let
        h = w/5

        linePoints : List (Float,Float)
        linePoints = [(0,h), (w/2,1), (w,h)]

        arrowHead : Float -> Svg Msg
        arrowHead offsetY =
            Svg.polyline
                [ SA.points <| pointsToString (L.map (\(x,y) -> (x,y + offsetY)) linePoints)
                , SA.stroke <| svgGray grays.lightgray
                , SA.strokeWidth <| fromFloat <| 2
                , SA.fillOpacity "0"
                ]
                []
    in
    html <| svg
        [ SA.width <| fromFloat w
        , SA.height <| fromFloat (2*h+1)
        ]
        [ arrowHead 0
        , arrowHead h
        ]


horzRule : Element Msg
horzRule = horzRule_ 1

horzRule_ : Int -> Element Msg
horzRule_ h =
        el
            [ centerY
            , width fill
            , Border.widthEach {top = 0, left = 0, right = 0, bottom = h}
            ]
            none


viewHelpButton : Int -> Element Msg
viewHelpButton radius =
    el
        [ padding <| radius
        , centerX
        , centerY
        , Border.rounded <| 5*radius
        , Background.color lightgray
        , Font.color darkgray
        , Font.size 36
        , onClick <| ShowHelp True
        , Border.shadow
            { offset = (3, 3)
            , size = 3
            , blur = 5
            , color = rgb 0 0 0
            }
        ]
        <| text "?"
    

viewScreen : RunState -> Element Msg
viewScreen ((state,currentTrial) as runState) = 
    let
        safeWidth = state.width + 2 * (drawPaddingX state)
    in
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
            [ SA.width <| fromFloat safeWidth
            , SA.height <| fromFloat state.height
            , SA.viewBox
                <| S.join " "
                    ( L.map fromFloat
                        [ 0
                        , 0
                        , safeWidth
                        , state.height
                        ]
                    )
            , SA.fill "rgb(51,51,51)"
            -- , centerX
            ]
            <| Svg.rect
                [SA.width <| fromFloat safeWidth
                , SA.height <| fromFloat state.height
                ]
                []
            :: drawCue runState
            ++ drawTarget runState
            ++ drawRocket runState
            ++ drawFeedback runState


drawPaddingX : ExperimentData -> Float
drawPaddingX state =
    (getRocketRadius state)


drawRocket : RunState -> List (Svg Msg)
drawRocket (state,currentTrial) =
        case currentTrial of
            Launched trialData ->
                let
                    rocket =
                        { x = trialData.xFrac * state.width + (drawPaddingX state)
                        , y = getRocketY trialData state
                        , r = getRocketRadius state
                        }
                in
                    Rocket.view rocket

            _ -> []


drawCue : RunState -> List (Svg Msg)
drawCue ((state,_) as runState) = 
    if state.cue then
        Svg.rect
            [ SA.x <| fromFloat <| getCueX runState + (drawPaddingX state)
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


drawTarget : RunState -> List (Svg Msg)
drawTarget ((state,currentTrial) as runState) =
    let
        x = getX runState + (drawPaddingX state)
    in
    if state.target then
        Svg.polygon
            [ SA.points <| pointsToString <|
                makeCrossPolygonPoints (x, getTargetY state) (getTargetSize state)
            , SA.stroke "rgb(255,255,255)"
            , SA.strokeWidth <| fromFloat <| (getTargetSize state) / 4
            ]
            []
        :: []
    else
        []


drawFeedback : RunState -> List (Svg Msg)
drawFeedback (state,currentTrial) =
        case currentTrial of
            Launched trialData ->
                let
                    size = getFeedbackSize state
                in
                    if hasEnded trialData then
                        if (result trialData) then
                            Svg.polyline
                                [ SA.points <| pointsToString <|
                                    makeCheckmarkPolygonPoints
                                        (state.width/2 + (drawPaddingX state), state.height/2)
                                        (getFeedbackSize state)
                                , SA.stroke "rgb(0,255,0)"
                                , SA.strokeWidth <| fromFloat <| (getFeedbackSize state) / 4
                                ]
                                []
                            :: Svg.text_
                                [ SA.x <| fromFloat <| (state.width/2 - size * 0.70) + (drawPaddingX state)
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
                                    makeCrossPolygonPoints
                                        (state.width/2 + (drawPaddingX state), state.height/2)
                                        size
                                , SA.stroke "rgb(255,0,0)"
                                , SA.strokeWidth <| fromFloat <| size / 4
                                ]
                                []
                            :: Svg.text_
                                [ SA.x <| fromFloat <| (state.width/2 - size * 0.70) + (drawPaddingX state)
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


getX : RunState -> Float
getX (state,currentTrial) =
    let 
        xFrac =
            case currentTrial of
                Idle data -> data.xFrac
                Waiting data -> data.xFrac
                Launchable data -> data.xFrac
                Launched data -> data.xFrac
    in
        xFrac * state.width


getCueX : RunState -> Float
getCueX ((state,_) as runState) =
    getX runState - (getCueWidth state) / 2


getStartY : ExperimentData -> Float
getStartY state =
    (1 - params.startYFrac) * state.height


getTargetY : ExperimentData -> Float
getTargetY state =
    (1 - params.targetYFrac) * state.height


getRocketY : TrialData -> ExperimentData -> Float
getRocketY trialData state =
    let
        dt = toFloat
            <| (posixToMillis trialData.currentTime)
            - (posixToMillis trialData.launchTime)

        dy = dt / params.flightDuration

        flightDist = (params.targetYFrac - params.startYFrac)
    in
        state.height * (1 - (params.startYFrac + flightDist * dy))


getRocketRadius : ExperimentData -> Float
getRocketRadius state =
    params.rocketSizeFrac * state.height


getCueWidth : ExperimentData -> Float
getCueWidth state =
    params.cueWidthFrac * state.height


getCueHeight : ExperimentData -> Float
getCueHeight state =
    (getCueWidth state) / 5


getTargetSize : ExperimentData -> Float
getTargetSize state =
    params.targetSizeFrac * state.height


getFeedbackSize : ExperimentData -> Float
getFeedbackSize state =
    params.feedbackSizeFrac * state.height


heightFillerW weight =
    el
        [ height <| fillPortion weight
        ]
        none


grays =
    { background = 0.2
    , gray = 0.6
    , lightgray = 0.8
    , darkgray = 0.4
    }

svgGray : Float -> String
svgGray t =
    let
        scaledTint = t * 255
        mkStrings = L.map fromFloat << L.repeat 3
    in
        "rgb(" ++ S.join "," (mkStrings scaledTint) ++ ")"

background = rgb 0.2 0.2 0.2
gray = rgb 0.6 0.6 0.6
lightgray = rgb 0.8 0.8 0.8
darkgray = rgb 0.4 0.4 0.4
white = rgb 1 1 1

graytint : Float -> Color
graytint f =
    rgb f f f

grayGradient : Float -> Float -> List (Color)
grayGradient min max =
    let
        tints = linSpace min max 20

        sep : Color -> Element msg
        sep color =
            el
                [ width <| fillPortion 3
                , Border.widthEach
                    { bottom=1
                    , left=0
                    , right=0
                    , top=0
                    }
                , Border.color color
                ]
                none

        leftPart = L.map graytint tints
    in
        leftPart ++ (L.reverse leftPart)

horzSeparator_ : Float -> Float -> Element msg
horzSeparator_ min max =
    let
        sep : Color -> Element msg
        sep color =
            el
                [ width <| fillPortion 3
                , Border.widthEach
                    { bottom=1
                    , left=0
                    , right=0
                    , top=0
                    }
                , Border.color color
                ]
                none
    in
        row [width fill] <| L.map sep <| grayGradient min max

horzSeparator : Element msg
horzSeparator = horzSeparator_ 0.25 0.55

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
        tints = [0.25, 0.275, 0.3, 0.325, 0.35, 0.375, 0.4, 0.425, 0.45]

    in
        column [height fill]
            ( [ el [height <| fillPortion 5] none
              ]
              ++ (L.map sep <| grayGradient 0.25 0.55)
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
                    , Background.color gray
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
            <| L.repeat 3 (dot radius)


menuPanel : Animator.Timeline Bool -> Maybe ExperimentData-> Element Msg
menuPanel timeline maybeState =
    let
        buttonWidth = 30
        buttonHeight = 48

        menuWidth = 325
        menuHeight = 420--390

        modifier = 
            Animator.linear timeline <|
                \b ->
                    if b then
                        Animator.at 1
                    else
                        Animator.at 0

        w = interpolate buttonWidth menuWidth modifier
        h = interpolate buttonHeight menuHeight modifier
        bg = fromRgb <| Color.toRgba <| Animator.color
                timeline
                ( \s ->
                    if s then
                        Color.rgb grays.lightgray grays.lightgray grays.lightgray
                    else
                        Color.rgb grays.background grays.background grays.background
                )
                
                

        onClickMsg = (MenuToggle (not <| Animator.current timeline))
        showMenu = Animator.current timeline
    in
    el
        [ width fill
        , height fill
        , behindContent <|
            el
                [ width fill
                , height fill
                , onClick BackgroundClick
                ]
                none
        ]
        <| 
        column
            [ Border.rounded 6
            , Background.color bg
            , Font.color background
            , alignTop
            , alignRight
            , width <| px <| round w
            , height <| px <| round h
            , clip
            ]
            ( if showMenu then
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
                        , menuButton background [alignRight, alignTop ] onClickMsg
                    ]
                , column
                    [ padding 15
                    , spacing 15
                    , centerX
                    , height fill
                    ]
                    [ button
                        [centerX]
                        { onPress = Just <| ToggleDebugging
                        , label = text "Toggle debugging"
                        }
                    , button
                        [centerX]
                        { onPress = Just <| DownloadDebugTrace
                        , label = text "Save debugging"
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
                     , button
                        [centerX]
                        { onPress = Just NoOp
                        , label = text "Help"
                        }
                    , el [height fill] none
                    , horzSeparator_ 0.65 0.2
                    , el [height fill] none
                   -- , el
                   --     [ paddingXY 0 20
                   --     , width fill
                   --     ]
                   --     <| el
                   --         [ Border.width 1
                   --         , Border.color gray
                   --         , width fill
                   --         ]
                   --         none
                    , row
                        [ width fill
                        , spacing 20
                        , paddingEach { bottom = 6, top = 0, left = 0, right = 0}
                        ]
                        [ column
                            [ alignLeft
                            , Font.light
                            , spacing 15
                            ]
                            [ text "Trials completed:"
                            , text "Session performance:"
                            , text "Session type:"
                            ]
                        , column
                            [ alignRight
                            , Font.medium
                            , spacing 15
                            ]
                            (case maybeState of
                                Just state ->
                                    [ text
                                        <| (fromInt <| (trialIdx state) - 1)
                                        ++ " of " ++ (fromInt <| nTrials state.sessionType)
                                    , text
                                        <| ((fromInt << round << (*) 100 << successRate) state.results) ++ "%"
                                    , (case state.sessionType of
                                        Train _ -> text "Train"
                                        Test _ -> text "Test"
                                    )
                                    ]
                                Nothing -> []
                            )
                        ]
                    ]
                ]
            else
                [ menuButton
                    lightgray
                    [ alignTop
                    , alignRight
                    ]
                    onClickMsg
                ]
            )



-- AUDIO


renderAudio : AudioData -> ProgramState -> Audio
renderAudio _ programState = 
    case programState of
        Running (state,_) ->
            case state.beep of
                Just beepSound ->
                    case state.beepAt of
                        Nothing -> silence
                        Just time ->
                            audio beepSound time

                Nothing -> silence

        _ ->
            silence



-- ANIMATOR


experimentAnimator : Animator.Animator ExperimentData
experimentAnimator =
    Animator.animator
        |> Animator.watching
            .showMenu
            (\newShowMenu model ->
                { model | showMenu = newShowMenu }
            )


helpVisibilityAnimator : Animator.Animator SetupState
helpVisibilityAnimator =
    Animator.animator
        |> Animator.watching
            .helpShown
            (\newShowHelp model ->
                { model | helpShown = newShowHelp }
            )


helpPageAnimator : Animator.Animator SetupState
helpPageAnimator =
    Animator.animator
        |> Animator.watching
            .helpPage
            (\nextHelpPage model ->
                { model | helpPage = nextHelpPage }
            )


textAlphaAnimator1 : Animator.Animator SetupState
textAlphaAnimator1 =
    Animator.animator
        |> Animator.watching
            .textAlpha
            (\newAlpha ({textAlpha} as model) ->
                { model | textAlpha = newAlpha }
            )

textAlphaAnimator2 : Animator.Animator ExperimentData
textAlphaAnimator2 =
    Animator.animator
        |> Animator.watching
            .textAlpha
            (\newAlpha ({textAlpha} as model) ->
                { model | textAlpha = newAlpha }
            )



-- UTILS

safeTail : List a -> List a
safeTail = Maybe.withDefault [] << L.tail

unlines : List String -> String
unlines =
    S.join "\u{000A}"


getListEntry : Int -> List a -> Maybe a
getListEntry i list =
    if i < 0 then
        Nothing
    else
        case list of
            it::items ->
                if i == 0 then
                    Just it
                else
                    getListEntry (i-1) items
            [] ->
                Nothing


linSpace : Float -> Float -> Int -> List Float
linSpace min max n =
    let
        delta = (max - min) / (toFloat (n-1))
    in
        L.map
            ((\idx -> min + delta * idx) << toFloat)
            <| L.range 0 (n-1)


interpolate : Float -> Float -> Float -> Float
interpolate min max frac =
    min + (max - min) * frac


fst : (a,b,c) -> a
fst (a,b,c) = a

snd : (a,b,c) -> b
snd (a,b,c) = b

thd : (a,b,c) -> c
thd (a,b,c) = c
