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
    }



-- Update


type Msg
    = Tick Time.Posix
    | StartChallenge
    | RoundDone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            case model.gameStatus of
                InProgress ->
                    ( model |> tickSecond, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StartChallenge ->
            ( model |> startChallenge, Cmd.none )

        RoundDone ->
            ( model |> advanceRound, Cmd.none )


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


getRestInSeconds : Model -> Int
getRestInSeconds model =
    if model.finishedRounds < 4 then
        30

    else if model.finishedRounds < 7 then
        60

    else
        120


getNextReps : Model -> Int
getNextReps model =
    if model.finishedRounds < model.maxReps then
        model.finishedRounds + 1

    else
        2 * model.maxReps - (model.finishedRounds + 1)


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

        newModel =
            { model
                | finishedRounds = model.finishedRounds + 1
                , roundStatus = newRoundStatus
                , gameStatus = newGameStatus
                , remainingRest = getRestInSeconds model
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
        [ Html.text (Debug.toString model)
        , Html.br [] []
        , Html.button [ onClick RoundDone ] [ Html.text "Round Done!" ]
        , Html.button [ onClick StartChallenge ] [ Html.text "Start Challenge!" ]
        ]



-- Init


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gameStatus = Init
      , roundStatus = None
      , finishedRounds = 0
      , maxReps = 10
      , totalTime = 0
      , remainingRest = 0
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
