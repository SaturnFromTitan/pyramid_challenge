module Model exposing (Exercise(..), Model, Status(..), getGameText, getMaxReps, getMaxRounds, getMaxTotalReps, getReps, getRest, getTotalReps, initialModel, isInProgress)

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


getMaxReps : Model -> Int
getMaxReps model =
    case model.exercise of
        Pushups ->
            10

        Pullups ->
            4


getMaxRounds : Model -> Int
getMaxRounds model =
    let
        maxReps =
            getMaxReps model
    in
    2 * maxReps - 1


getMaxTotalReps : Model -> Int
getMaxTotalReps model =
    let
        maxReps =
            getMaxReps model
    in
    2 * sumOf1To maxReps - maxReps


getTotalReps : Model -> Int
getTotalReps model =
    let
        maxReps =
            getMaxReps model

        maxRounds =
            getMaxRounds model

        maxTotalReps =
            getMaxTotalReps model
    in
    if model.finishedRounds <= maxReps then
        sumOf1To model.finishedRounds

    else
        maxTotalReps - sumOf1To (maxRounds - model.finishedRounds)


getReps : Model -> Int -> Int
getReps model finishedRounds =
    let
        maxReps =
            getMaxReps model
    in
    if finishedRounds <= maxReps then
        finishedRounds

    else
        2 * maxReps - finishedRounds


maxRest : Int
maxRest =
    60


getRest : Model -> Int -> Int
getRest model finishedRounds =
    let
        restAmount =
            case model.exercise of
                Pushups ->
                    getRestPushups model finishedRounds

                Pullups ->
                    getRestPullups model finishedRounds
    in
    min maxRest restAmount


getRestPushups : Model -> Int -> Int
getRestPushups model finishedRounds =
    let
        finishedReps =
            getReps model finishedRounds
    in
    if finishedReps <= 3 then
        5

    else
        (finishedReps - 1) * 10


getRestPullups : Model -> Int -> Int
getRestPullups model finishedRounds =
    let
        finishedReps =
            getReps model finishedRounds
    in
    if finishedReps <= 2 then
        30

    else
        maxRest


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
