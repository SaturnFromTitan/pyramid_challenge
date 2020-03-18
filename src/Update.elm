module Update exposing (Msg(..), subscriptions, update)

import Keyboard exposing (Key(..), RawKey)
import Model exposing (Model, getMaxRounds, getRest, isInProgress)
import String
import Time
import Utilities exposing (sumOf1To)


type Msg
    = Tick Time.Posix
    | StartChallenge Model.Exercise
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
        newModel =
            case msg of
                Tick _ ->
                    if isInProgress model then
                        model |> tickSecond

                    else
                        model

                StartChallenge exercise ->
                    model |> startChallenge exercise

                RoundDone ->
                    model |> advanceRound

                KeyDown _ ->
                    model |> advanceRound
    in
    ( newModel, Cmd.none )


tickSecond : Model -> Model
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
    in
    { model
        | totalTime = model.totalTime + 1
        , remainingRest = newRemainingRest
        , status = newStatus
    }


startChallenge : Model.Exercise -> Model -> Model
startChallenge exercise model =
    { model
        | status = Model.Doing
        , exercise = exercise
    }


advanceRound : Model -> Model
advanceRound model =
    let
        isPushing =
            model.status == Model.Doing

        newFinishedRounds =
            model.finishedRounds + 1

        wasLastRound =
            newFinishedRounds == getMaxRounds model

        newStatus =
            if wasLastRound then
                Model.Finished

            else
                Model.Resting
    in
    if isPushing then
        { model
            | finishedRounds = newFinishedRounds
            , status = newStatus
            , remainingRest = getRest model newFinishedRounds
        }

    else
        model
