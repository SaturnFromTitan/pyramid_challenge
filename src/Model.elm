module Model exposing (Exercise(..), Model, Status(..), getGameText, getMaxRepsPerRound, getRepsOfRound, getRestAfterRound, getTotalFinishedReps, getTotalReps, getTotalRounds, initialModel, isInProgress)

import Sound exposing (Sound)
import Utilities exposing (sumOf1To)


type Status
    = Init
    | Doing
    | Resting
    | Finished


type Exercise
    = Pushups
    | Pullups


type alias Model =
    { status : Status
    , exercise : Exercise
    , finishedRounds : Int
    , totalTime : Int
    , remainingRest : Int
    }


initialModel : Model
initialModel =
    { status = Init
    , exercise = Pushups
    , finishedRounds = 0
    , totalTime = 0
    , remainingRest = 0
    }


isInProgress : Model -> Bool
isInProgress model =
    List.member model.status [ Doing, Resting ]


getMaxRepsPerRound : Model -> Int
getMaxRepsPerRound model =
    case model.exercise of
        Pushups ->
            10

        Pullups ->
            6


getTotalRounds : Model -> Int
getTotalRounds model =
    let
        maxReps =
            getMaxRepsPerRound model
    in
    2 * maxReps - 1


getTotalFinishedReps : Model -> Int
getTotalFinishedReps model =
    let
        maxReps =
            getMaxRepsPerRound model
    in
    2 * sumOf1To maxReps - maxReps


getTotalReps : Model -> Int
getTotalReps model =
    let
        maxReps =
            getMaxRepsPerRound model

        maxRounds =
            getTotalRounds model

        maxTotalReps =
            getTotalFinishedReps model
    in
    if model.finishedRounds <= maxReps then
        sumOf1To model.finishedRounds

    else
        maxTotalReps - sumOf1To (maxRounds - model.finishedRounds)


getRepsOfRound : Model -> Int -> Int
getRepsOfRound model finishedRounds =
    let
        maxReps =
            getMaxRepsPerRound model
    in
    if finishedRounds <= maxReps then
        finishedRounds

    else
        2 * maxReps - finishedRounds


maxRest : Exercise -> Int
maxRest exercise =
    case exercise of
        Pushups ->
            50

        Pullups ->
            90


getRestAfterRound : Model -> Int -> Int
getRestAfterRound model finishedRounds =
    let
        nextRound =
            finishedRounds + 1

        repsNextRound =
            getRepsOfRound model nextRound

        getRestFunc =
            case model.exercise of
                Pushups ->
                    getRestPushups

                Pullups ->
                    getRestPullups

        restAmount =
            getRestFunc model repsNextRound

        maxRestAmount =
            maxRest model.exercise
    in
    min maxRestAmount restAmount


getRestPushups : Model -> Int -> Int
getRestPushups model repsNextRound =
    if repsNextRound <= 4 then
        3

    else
        (repsNextRound - 3) * 10


getRestPullups : Model -> Int -> Int
getRestPullups model repsNextRound =
    (repsNextRound - 1) * 15


getGameText : Model -> String
getGameText model =
    case model.status of
        Init ->
            "The Pyramid Challenge"

        Doing ->
            case model.exercise of
                Pushups ->
                    "Push, push, push!"

                Pullups ->
                    "Pull, pull, pull!"

        Resting ->
            "Pace yourself..."

        Finished ->
            "You did it! Congrats!"
