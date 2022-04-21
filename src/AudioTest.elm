port module AudioTest exposing (..)

import Audio exposing
    ( Audio
    , AudioCmd
    , AudioData
    , audio
    , silence
    , elementWithAudio
    )
import Element exposing (..)
import Element.Input exposing (button)
import Html exposing (Html)
import Json.Decode
import Json.Encode
import Time exposing (Posix)
import Task


port audioPortToJS : Json.Encode.Value -> Cmd msg
port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg

main : Platform.Program () (Audio.Model Msg Model) (Audio.Msg Msg)
main = elementWithAudio
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , audio = renderAudio
        , audioPort = { toJS = audioPortToJS, fromJS = audioPortFromJS }
        }

type Model
    = LoadingModel
    | LoadedModel LoadedModel_
    | FailedModel

type alias LoadedModel_ =
    { sound : Audio.Source
    , soundState : SoundState
    }


type SoundState
    = Silent
    | Playing Posix


init : flags -> ( Model, Cmd Msg, AudioCmd Msg )
init _ = 
    ( LoadingModel
    , Cmd.none
    , Audio.loadAudio
        SoundLoaded
            "beep.wav"

    )



type Msg
    = SoundLoaded (Result Audio.LoadError Audio.Source)
    | PlayPause
    | ToggleTime Posix


update : AudioData -> Msg -> Model -> (Model, Cmd Msg, AudioCmd Msg)
update _ msg model =
    case (msg,model) of
        ( SoundLoaded result, LoadingModel ) ->
            case result of
                Ok sound ->
                    ( LoadedModel { sound = sound, soundState = Silent }
                    , Cmd.none
                    , Audio.cmdNone
                    )

                Err _ ->
                    ( FailedModel
                    , Cmd.none
                    , Audio.cmdNone
                    )

        ( PlayPause, LoadedModel loadedModel ) ->
            ( model
            , Task.perform ToggleTime Time.now
            , Audio.cmdNone
            )

        ( ToggleTime time, LoadedModel loadedModel ) ->
            let
                newState =
                    case loadedModel.soundState of
                        Silent -> Playing time
                        Playing _ -> Silent
            in
            ( LoadedModel { loadedModel | soundState = newState }
            , Cmd.none
            , Audio.cmdNone
            )


        _ -> (model, Cmd.none, Audio.cmdNone)


subscriptions : AudioData -> Model -> Sub Msg
subscriptions _ _ = Sub.none


view : AudioData -> Model -> Html Msg
view _ model =
    case model of
        LoadingModel ->
            layout [] <| text "Loading audio ..."
        LoadedModel loadedModel ->
            layout [] <| button [] {onPress = Just PlayPause, label = text "Play/Pause" }
        FailedModel ->
            layout [] <| text "Failed to load audio."


renderAudio : AudioData -> Model -> Audio
renderAudio _ model = 
    case model of
        LoadedModel loadedModel ->
            case loadedModel.soundState of
                Silent -> silence
                Playing time ->
                    audio loadedModel.sound time

        _ ->
            silence


