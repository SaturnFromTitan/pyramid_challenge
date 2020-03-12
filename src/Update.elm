module Update exposing (Msg(..), subscriptions, update)

import Keyboard exposing (Key(..), RawKey)
import Model exposing (Model, getRest, isInProgress, maxRounds)
import String
import Time
import Utilities exposing (sumOf1To)


type Msg
    = Tick Time.Posix
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
        isPushing =
            model.status == Model.Pushing

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
    if isPushing then
        { model
            | finishedRounds = newFinishedRounds
            , status = newStatus
            , remainingRest = getRest newFinishedRounds
        }

    else
        model
