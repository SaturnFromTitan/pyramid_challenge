module Update exposing (Msg(..), subscriptions, update)

import Keyboard exposing (Key(..), RawKey)
import Model exposing (Model, getRest, maxRounds)
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

                KeyDown rawKey ->
                    let
                        key =
                            Keyboard.anyKeyOriginal rawKey
                    in
                    case key of
                        Just Spacebar ->
                            model |> advanceRound

                        _ ->
                            model
    in
    ( newModel, Cmd.none )


isInProgress : Model -> Bool
isInProgress model =
    List.member model.status [ Model.Pushing, Model.Resting ]


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
