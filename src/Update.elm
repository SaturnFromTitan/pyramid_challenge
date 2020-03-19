module Update exposing (Msg(..), subscriptions, update)

import Keyboard exposing (Key(..), RawKey)
import Model exposing (Model, getRestAfterRound, getTotalRounds, isInProgress)
import Sound exposing (Sound, play)
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
        ( newModel, sounds ) =
            case msg of
                Tick _ ->
                    if isInProgress model then
                        model |> tickSecond

                    else
                        ( model, [] )

                StartChallenge exercise ->
                    model |> startChallenge exercise

                RoundDone ->
                    model |> advanceRound

                KeyDown _ ->
                    model |> advanceRound
    in
    ( newModel
    , Cmd.batch <| List.map Sound.play sounds
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


startChallenge : Model.Exercise -> Model -> ( Model, List Sound )
startChallenge exercise model =
    ( { model
        | status = Model.Doing
        , exercise = exercise
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
