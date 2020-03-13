module Model exposing (Model, Status(..), getGameText, getReps, getRest, getTotalReps, initialModel, isInProgress, maxReps, maxRounds, maxTotalReps)

import Utilities exposing (sumOf1To)


type Status
    = Init
    | Pushing
    | Resting
    | Finished


type alias Model =
    { status : Status
    , finishedRounds : Int
    , totalTime : Int
    , remainingRest : Int
    }


initialModel : Model
initialModel =
    { status = Init
    , finishedRounds = 0
    , totalTime = 0
    , remainingRest = 0
    }


isInProgress : Model -> Bool
isInProgress model =
    List.member model.status [ Pushing, Resting ]


maxReps : Int
maxReps =
    10


maxRounds : Int
maxRounds =
    2 * maxReps - 1


maxTotalReps : Int
maxTotalReps =
    2 * sumOf1To maxReps - maxReps


maxRest : Int
maxRest =
    60


getRest : Int -> Int
getRest finishedRounds =
    let
        finishedReps =
            getReps finishedRounds

        restAmount =
            if finishedReps <= 3 then
                5

            else
                (finishedReps - 1) * 10
    in
    min maxRest restAmount


getTotalReps : Int -> Int
getTotalReps finishedRounds =
    if finishedRounds <= maxReps then
        sumOf1To finishedRounds

    else
        maxTotalReps - sumOf1To (maxRounds - finishedRounds)


getReps : Int -> Int
getReps finishedRounds =
    if finishedRounds <= maxReps then
        finishedRounds

    else
        2 * maxReps - finishedRounds


getGameText : Model -> String
getGameText model =
    case model.status of
        Init ->
            "100 Pushups?!"

        Pushing ->
            "Push, push, push!"

        Resting ->
            "Pace yourself..."

        Finished ->
            "You did it! Congrats!"
