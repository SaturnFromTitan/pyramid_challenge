module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Debug
import Html exposing (Html)
import Html.Events exposing (onClick)
import String
import Time



-- Model


type GameStatus
    = Init
    | InProgress
    | Finished


type RoundStatus
    = None
    | Pushing
    | Rest


type alias Model =
    { gameStatus : GameStatus
    , roundStatus : RoundStatus
    , finishedRounds : Int
    , totalTime : Int
    , remainingRest : Int
    , maxReps : Int
    , gameText : String
    }



-- Update


type Msg
    = Tick Time.Posix
    | StartChallenge
    | RoundDone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel_ =
            case msg of
                Tick _ ->
                    case model.gameStatus of
                        InProgress ->
                            model |> tickSecond

                        _ ->
                            model

                StartChallenge ->
                    model |> startChallenge

                RoundDone ->
                    model |> advanceRound

        newModel =
            { newModel_ | gameText = getGameText newModel_ }
    in
    ( newModel, Cmd.none )


tickSecond : Model -> Model
tickSecond model =
    let
        newRemainingRest =
            max 0 (model.remainingRest - 1)

        restIsOver =
            newRemainingRest == 0

        newRoundStatus =
            if restIsOver then
                Pushing

            else
                Rest
    in
    { model
        | totalTime = model.totalTime + 1
        , remainingRest = newRemainingRest
        , roundStatus = newRoundStatus
    }


maxRound : Model -> Int
maxRound model =
    2 * model.maxReps - 1


getGameText : Model -> String
getGameText model =
    if model.gameStatus == Init then
        initGameText

    else if model.gameStatus == Finished then
        "You did it! Congrats!"

    else if model.roundStatus == Pushing then
        "Uh yeah, push it!"

    else if model.roundStatus == Rest then
        "Pace yourself and get some rest"

    else
        "42"


getRestInSeconds : Int -> Int -> Int
getRestInSeconds finishedRounds maxRounds =
    if finishedRounds == maxRounds then
        0

    else if finishedRounds < 4 then
        30

    else if finishedRounds < 7 then
        60

    else
        120


getMaxTotalReps : Int -> Int
getMaxTotalReps maxReps =
    (maxReps * (maxReps + 1)) - maxReps


getTotalReps : Int -> Int -> Int
getTotalReps finishedRounds maxReps =
    let
        r =
            finishedRounds

        m =
            maxReps
    in
    if r <= m then
        r * (r + 1) // 2

    else
        100 - ((2 * m - 1) - r) * ((2 * m) - r) // 2


getNextReps : Int -> Int -> Int
getNextReps finishedRounds maxReps =
    if finishedRounds < maxReps then
        finishedRounds + 1

    else
        2 * maxReps - (finishedRounds + 1)


startChallenge : Model -> Model
startChallenge model =
    let
        newModel =
            { model
                | gameStatus = InProgress
                , roundStatus = Pushing
            }
    in
    if isValidStartChallenge model newModel then
        newModel

    else
        model


isValidStartChallenge : Model -> Model -> Bool
isValidStartChallenge model newModel =
    let
        initToInProgress =
            (model.gameStatus == Init) && (newModel.gameStatus == InProgress)

        inProgressToFinished =
            (model.gameStatus == InProgress) && (newModel.gameStatus == Finished)
    in
    initToInProgress || inProgressToFinished


advanceRound : Model -> Model
advanceRound model =
    let
        newFinishedRounds =
            model.finishedRounds + 1

        wasLastRound =
            newFinishedRounds == maxRound model

        newRoundStatus =
            if wasLastRound then
                None

            else
                Rest

        newGameStatus =
            if wasLastRound then
                Finished

            else
                InProgress

        maxRounds =
            maxRound model

        newModel =
            { model
                | finishedRounds = newFinishedRounds
                , roundStatus = newRoundStatus
                , gameStatus = newGameStatus
                , remainingRest = getRestInSeconds newFinishedRounds maxRounds
            }
    in
    if isValidNextRound model newModel then
        newModel

    else
        model


isValidNextRound : Model -> Model -> Bool
isValidNextRound model newModel =
    let
        gameIsStarted =
            model.gameStatus == InProgress

        isValidRoundTransition =
            newModel.finishedRounds <= maxRound newModel
    in
    gameIsStarted && isValidRoundTransition



-- View


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.text "Total time: "
        , Html.text (String.fromInt model.totalTime)
        , Html.br [] []
        , Html.text "Total reps: "
        , Html.text (String.fromInt (getTotalReps model.finishedRounds model.maxReps))
        , Html.text "/"
        , Html.text (String.fromInt (getMaxTotalReps model.maxReps))
        , Html.br [] []
        , Html.text "Next reps: "
        , Html.text (String.fromInt (getNextReps model.finishedRounds model.maxReps))
        , Html.br [] []
        , Html.text "Remaining rest: "
        , Html.text (String.fromInt model.remainingRest)
        , Html.br [] []
        , Html.button [ onClick RoundDone ] [ Html.text "Round Done!" ]
        , Html.button [ onClick StartChallenge ] [ Html.text "Start Challenge!" ]
        , Html.br [] []
        , Html.text model.gameText

        -- , Html.br [] []
        -- , Html.text (Debug.toString model)
        ]



-- Init


initGameText =
    "Ready for some pushups?!"


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gameStatus = Init
      , roundStatus = None
      , finishedRounds = 0
      , maxReps = 10
      , totalTime = 0
      , remainingRest = 0
      , gameText = initGameText
      }
    , Cmd.none
    )



-- Subscription


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- Main


main : Program () Model Msg
main =
    Browser.element
        { update = update
        , view = view
        , init = init
        , subscriptions = subscriptions
        }
