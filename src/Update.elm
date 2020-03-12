module Update exposing (Msg(..), subscriptions, update)

import Browser.Events exposing (onKeyPress)
import Json.Decode as Decode
import Model exposing (Model, getRest, isInProgress, maxRounds)
import String
import Time
import Utilities exposing (sumOf1To)


type Msg
    = Tick Time.Posix
    | StartChallenge
    | RoundDone
    | KeyPressed


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyPress (Decode.succeed KeyPressed)
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

                StartChallenge ->
                    model |> startChallenge

                RoundDone ->
                    model |> advanceRound

                KeyPressed ->
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
                Model.Pushing

            else
                Model.Resting
    in
    { model
        | totalTime = model.totalTime + 1
        , remainingRest = newRemainingRest
        , status = newStatus
    }


startChallenge : Model -> Model
startChallenge model =
    { model | status = Model.Pushing }


advanceRound : Model -> Model
advanceRound model =
    let
        newFinishedRounds =
            model.finishedRounds + 1

        wasLastRound =
            newFinishedRounds == maxRounds

        newStatus =
            if wasLastRound then
                Model.Finished

            else
                Model.Resting
    in
    if isInProgress model then
        { model
            | finishedRounds = newFinishedRounds
            , status = newStatus
            , remainingRest = getRest newFinishedRounds
        }

    else
        model
