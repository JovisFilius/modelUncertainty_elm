module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyPress)
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

import String exposing (fromInt)
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


type alias Model =
    { params : Params
    , results : List Trial
    , currentTrial : Maybe Trial
    , distModel : DistModel
    , rocketY : Maybe Float
    , debugStr : String
    , cue : Bool
    , target : Bool
    }


type alias Params =
    { trialDelayMin : Int
    , trialDelayMax : Int
    , cueDuration : Int
    , targetDuration : Int
    , nTrials : Int
    , stepY : Float
    , stepT : Int
    , rocketSize : Float
    , targetSize : Float
    , cueWidth : Float
    }


type alias Trial =
    { startTime : Posix
    , launchTime : Posix
    , arrivalTime : Posix
    , initialDelay : Posix
    , x : Float
    , profile : Profile
    }


type Profile
    = Constant
    | Linear
    | Quadratic


type alias DistModel = Random.Generator Profile


init : () -> (Model, Cmd Msg)
init _ =
    ( { params = defaultParams
      , results = []
      , currentTrial = Nothing
      , distModel = Random.weighted (100, Constant) []
      , rocketY = Nothing
      , debugStr = "foo"
      , cue = False
      , target = False
      }
    , Random.generate NewTrial (Random.float 0 1)
    )


defaultParams : Params
defaultParams =
    { trialDelayMin = 150
    , trialDelayMax = 1000
    , cueDuration = 350
    , targetDuration = 150
    , nTrials = 80
    , stepY = 0.1
    , stepT = 1
    , rocketSize = 0.0075
    , targetSize = 0.0075
    , cueWidth = 0.0375
    }


-- UPDATE


type Msg
    = SpacePressed
    | KeyPressed String
    | InitTrial Int
    | StartTrial Posix
    | HideCue
    | StepRocket
    | NewTrial Float
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTrial x ->
            ( { model | currentTrial = Just <| newTrial x }
            , Cmd.none
            )

        SpacePressed ->
            ( { model | debugStr = model.debugStr ++ "." }
            , Random.generate InitTrial (Random.int 150 1000)
            )
            -- , Task.perform InitTrial Time.now

        InitTrial t ->
            ( { model
              | debugStr = model.debugStr ++ "InitTrial[" ++ (fromInt t) ++ "]" }
            , Task.perform StartTrial (Process.sleep (toFloat t) |> andThen (\_ -> now))
            )

        StartTrial t ->
            case model.currentTrial of
                Just trial ->
                    ( { model
                      | currentTrial = Just <| startTrial t trial
                      , cue = True
                      , debugStr = model.debugStr ++ "StartTrial"
                      }
                    , Task.perform (\_ -> HideCue) (Process.sleep (toFloat model.params.cueDuration))
                    )

                _ ->
                    ( model, Cmd.none )

        HideCue ->
            ( { model | cue = False }
            , Cmd.none
            )

        KeyPressed str ->
            ( { model | debugStr = str }
            , Cmd.none
            )

        StepRocket ->
            case model.rocketY of
                Just y ->
                    ( { model | rocketY = Just <| y + model.params.stepY }
                    , Cmd.none
                    )
                Nothing ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


newTrial : Float -> Trial
newTrial x =
    { startTime = Time.millisToPosix 0
    , launchTime = Time.millisToPosix 0
    , arrivalTime = Time.millisToPosix 0
    , initialDelay = Time.millisToPosix 500
    , x = x
    , profile = Constant
    }


startTrial : Posix -> Trial -> Trial
startTrial time trial = { trial | startTime = time }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyPress <| keyDecoder


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
            , screen model
            , heightFiller
            , el [centerX] <| text <| "[" ++ model.debugStr ++ "]"
            ]


screen : Model -> Element Msg
screen model = 
    el
        [ width <| px 960
        , height <| px 740
        , Background.color background
        , centerX
        ]
        <| viewCue model.cue


viewCue : Bool -> Element Msg
viewCue c = 
    if c then
        el
            [ Font.color <| rgb 1 1 1
            , centerX
            , centerY
            ]
            <| text "cue"
    else
        none


heightFiller =
    el
        [ height fill
        ]
        none


background = rgb 0.2 0.2 0.2
