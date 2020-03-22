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
            4


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
            60


getRestAfterRound : Model -> Int -> Int
getRestAfterRound model finishedRounds =
    let
        restAmount =
            case model.exercise of
                Pushups ->
                    getRestAfterRoundPushups model finishedRounds

                Pullups ->
                    getRestAfterRoundPullups model finishedRounds
    in
    min (maxRest model.exercise) restAmount


getRestAfterRoundPushups : Model -> Int -> Int
getRestAfterRoundPushups model finishedRounds =
    let
        finishedReps =
            getRepsOfRound model finishedRounds
    in
    if finishedReps <= 3 then
        5

    else
        (finishedReps - 2) * 10


getRestAfterRoundPullups : Model -> Int -> Int
getRestAfterRoundPullups model finishedRounds =
    let
        finishedReps =
            getRepsOfRound model finishedRounds
    in
    if finishedReps <= 2 then
        30

    else
        maxRest model.exercise


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
