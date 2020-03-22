module Update exposing (Msg(..), subscriptions, update)

import Keyboard exposing (Key(..), RawKey)
import Model exposing (Model, getRestAfterRound, getTotalRounds, isInProgress)
import Sound exposing (Sound, play)
import String
import Time
import Utilities exposing (sumOf1To)


type Msg
    = Tick Time.Posix
    | SetExercise Model.Exercise
    | StartChallenge
    | RoundDone
    | KeyDown RawKey


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Time.every 1000 Tick
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, sounds ) =
            case msg of
                SetExercise exercise ->
                    model |> setExercise exercise

                Tick _ ->
                    if isInProgress model then
                        model |> tickSecond

                    else
                        ( model, [] )

                StartChallenge ->
                    model |> startChallenge

                RoundDone ->
                    model |> advanceRound

                KeyDown _ ->
                    model |> advanceRound
    in
    ( newModel
    , Cmd.batch <| List.map Sound.play sounds
    )


setExercise : Model.Exercise -> Model -> ( Model, List Sound )
setExercise exercise model =
    ( { model
        | exercise = exercise
      }
    , []
    )


tickSecond : Model -> ( Model, List Sound )
tickSecond model =
    let
        newRemainingRest =
            max 0 (model.remainingRest - 1)

        restIsOver =
            newRemainingRest == 0

        newStatus =
            if restIsOver then
                Model.Doing

            else
                Model.Resting

        sounds =
            if newRemainingRest == 3 then
                [ Sound.Countdown ]

            else
                []
    in
    ( { model
        | totalTime = model.totalTime + 1
        , remainingRest = newRemainingRest
        , status = newStatus
      }
    , sounds
    )


startChallenge : Model -> ( Model, List Sound )
startChallenge model =
    ( { model
        | status = Model.Doing
      }
    , []
    )


advanceRound : Model -> ( Model, List Sound )
advanceRound model =
    let
        isPushing =
            model.status == Model.Doing

        newFinishedRounds =
            model.finishedRounds + 1

        wasLastRound =
            newFinishedRounds == getTotalRounds model

        newStatus =
            if wasLastRound then
                Model.Finished

            else
                Model.Resting

        newModel =
            if isPushing then
                { model
                    | finishedRounds = newFinishedRounds
                    , status = newStatus
                    , remainingRest = getRestAfterRound model newFinishedRounds
                }

            else
                model
    in
    ( newModel, [] )
