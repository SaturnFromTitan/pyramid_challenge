module Main exposing (main)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Text as Text
import Browser
import Debug
import Html exposing (Html)
import Html.Events exposing (onClick)
import String
import Time



-- Helpers


sumOf1To : Int -> Int
sumOf1To n =
    -- Gaussian sum formula
    n * (n + 1) // 2



-- Model


maxReps : Int
maxReps =
    10


maxRounds : Int
maxRounds =
    2 * maxReps - 1


maxTotalReps : Int
maxTotalReps =
    2 * sumOf1To maxReps - maxReps


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


getRest : Int -> Int
getRest finishedRounds =
    let
        nextReps =
            getNextReps finishedRounds
    in
    if nextReps == 0 then
        0

    else if nextReps < 4 then
        30

    else if nextReps < 7 then
        60

    else
        120


getTotalReps : Int -> Int
getTotalReps finishedRounds =
    if finishedRounds <= maxReps then
        sumOf1To finishedRounds

    else
        maxTotalReps - sumOf1To (maxRounds - finishedRounds)


getNextReps : Int -> Int
getNextReps finishedRounds =
    if finishedRounds < maxReps then
        finishedRounds + 1

    else
        maxRounds - finishedRounds


startChallenge : Model -> Model
startChallenge model =
    let
        newModel =
            { model
                | gameStatus = InProgress
                , roundStatus = Pushing
            }
    in
    if isValidStart model newModel then
        newModel

    else
        model


isValidStart : Model -> Model -> Bool
isValidStart model newModel =
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
            newFinishedRounds == maxRounds

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
                | finishedRounds = newFinishedRounds
                , roundStatus = newRoundStatus
                , gameStatus = newGameStatus
                , remainingRest = getRest newFinishedRounds
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
            newModel.finishedRounds <= maxRounds
    in
    gameIsStarted && isValidRoundTransition



-- View


view : Model -> Html Msg
view model =
    Html.div []
        [ CDN.stylesheet
        , Grid.container []
            [ gameTextRow model
            , totalTimeRow model
            , totalRepsRow model
            , restRow model
            , nextRepsRow model
            , buttonsRow
            ]
        ]


makeRow : Html msg -> Html msg -> Html msg
makeRow content1 content2 =
    Grid.row [ Row.centerXs ]
        [ Grid.col
            [ Col.xs2 ]
            [ content1 ]
        , Grid.col
            [ Col.xs2, Col.textAlign Text.alignXsLeft ]
            [ content2 ]
        ]


totalTimeRow : Model -> Html msg
totalTimeRow model =
    makeRow (Html.text "Total time:") (Html.text (String.fromInt model.totalTime))


totalRepsRow : Model -> Html msg
totalRepsRow model =
    makeRow (Html.text "Total reps:") (Html.text (getRepsStatusText model))


restRow : Model -> Html msg
restRow model =
    makeRow (Html.text "Remaining rest:") (Html.text (String.fromInt model.remainingRest))


nextRepsRow : Model -> Html msg
nextRepsRow model =
    makeRow (Html.text "Next reps:") (Html.text (String.fromInt (getNextReps model.finishedRounds)))


getRepsStatusText : Model -> String
getRepsStatusText model =
    String.concat
        [ String.fromInt (getTotalReps model.finishedRounds)
        , " of "
        , String.fromInt maxTotalReps
        ]


buttonsRow : Html Msg
buttonsRow =
    makeRow
        (Button.submitButton [ Button.primary, Button.onClick RoundDone ] [ Html.text "Round Done!" ])
        (Button.submitButton [ Button.primary, Button.onClick StartChallenge ] [ Html.text "Let's go!" ])


gameTextRow : Model -> Html msg
gameTextRow model =
    Grid.row [ Row.centerXs ]
        [ Grid.col [ Col.xs4, Col.textAlign Text.alignXsCenter ]
            [ Html.text model.gameText ]
        ]



-- Init


initGameText =
    "Ready for some pushups?!"


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gameStatus = Init
      , roundStatus = None
      , finishedRounds = 0
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
