module Main exposing (main)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Progress as Progress
import Bootstrap.Text as Text
import Bootstrap.Utilities.Spacing as Spacing
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



-- Update


type Msg
    = Tick Time.Posix
    | StartChallenge
    | RoundDone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                Tick _ ->
                    case model.status of
                        Pushing ->
                            model |> tickSecond

                        Resting ->
                            model |> tickSecond

                        _ ->
                            model

                StartChallenge ->
                    model |> startChallenge

                RoundDone ->
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
                Pushing

            else
                Resting
    in
    { model
        | totalTime = model.totalTime + 1
        , remainingRest = newRemainingRest
        , status = newStatus
    }


getGameText : Model -> String
getGameText model =
    case model.status of
        Init ->
            "Ready for some Pushups?!"

        Pushing ->
            "Push, push, push!"

        Resting ->
            "Pace yourself and get some rest..."

        Finished ->
            "You did it! Congrats!"


getRest : Int -> Int
getRest finishedRounds =
    let
        finishedReps =
            getReps finishedRounds

        multiplicator =
            if finishedReps <= 3 then
                5

            else if finishedReps <= 6 then
                10

            else
                20
    in
    finishedReps * multiplicator


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


startChallenge : Model -> Model
startChallenge model =
    { model | status = Pushing }


advanceRound : Model -> Model
advanceRound model =
    let
        newFinishedRounds =
            model.finishedRounds + 1

        wasLastRound =
            newFinishedRounds == maxRounds

        newStatus =
            if wasLastRound then
                Finished

            else
                Resting
    in
    { model
        | finishedRounds = newFinishedRounds
        , status = newStatus
        , remainingRest = getRest newFinishedRounds
    }



-- View


view : Model -> Html Msg
view model =
    let
        elements =
            case model.status of
                Init ->
                    [ gameTextRow model
                    , buttonsRow model
                    ]

                Pushing ->
                    [ gameTextRow model
                    , totalRepsRow model
                    , nextRepsRow model
                    , buttonsRow model
                    ]

                Resting ->
                    [ gameTextRow model
                    , restRow model
                    ]

                Finished ->
                    [ gameTextRow model
                    , totalTimeRow model
                    ]
    in
    Html.div []
        [ CDN.stylesheet
        , Grid.container [] elements
        ]


makeRow : List (Html msg) -> Html msg
makeRow elements =
    Grid.row
        [ Row.centerXs, Row.attrs [ Spacing.p1 ] ]
        [ Grid.col
            [ Col.xs4, Col.textAlign Text.alignXsCenter ]
            elements
        ]


as2DigitString : Int -> String
as2DigitString num =
    let
        numAsString =
            String.fromInt num
    in
    if String.length numAsString == 1 then
        "0" ++ numAsString

    else
        numAsString


secondsToTime : Int -> String
secondsToTime totalSeconds =
    let
        hours =
            totalSeconds // 3600

        remainder =
            remainderBy 3600 totalSeconds

        minutes =
            remainder // 60

        seconds =
            remainderBy 60 remainder
    in
    String.concat
        [ as2DigitString hours
        , ":"
        , as2DigitString minutes
        , ":"
        , as2DigitString seconds
        ]


totalTimeRow : Model -> Html msg
totalTimeRow model =
    makeRow [ Html.text (secondsToTime model.totalTime) ]


totalRepsRow : Model -> Html msg
totalRepsRow model =
    let
        finishedReps =
            toFloat (getTotalReps model.finishedRounds)

        progress =
            Progress.progress
                [ Progress.success
                , Progress.value finishedReps
                ]
    in
    makeRow [ progress ]


getRepsStatusText : Model -> String
getRepsStatusText model =
    String.concat
        [ String.fromInt (getTotalReps model.finishedRounds)
        , " of "
        , String.fromInt maxTotalReps
        ]


restRow : Model -> Html msg
restRow model =
    makeRow [ Html.text (secondsToTime model.remainingRest) ]


nextRepsRow : Model -> Html msg
nextRepsRow model =
    let
        message =
            String.concat
                [ "Next reps: "
                , String.fromInt (getReps (model.finishedRounds + 1))
                ]
    in
    makeRow [ Html.text message ]


buttonsRow : Model -> Html Msg
buttonsRow model =
    let
        buttons =
            case model.status of
                Init ->
                    [ Button.submitButton [ Button.primary, Button.onClick StartChallenge ] [ Html.text "Start Challenge!" ] ]

                Pushing ->
                    [ Button.submitButton [ Button.primary, Button.onClick RoundDone ] [ Html.text "Round Done!" ] ]

                _ ->
                    []
    in
    makeRow buttons


gameTextRow : Model -> Html msg
gameTextRow model =
    makeRow [ Html.text (getGameText model) ]



-- Init


init : () -> ( Model, Cmd Msg )
init _ =
    ( { status = Init
      , finishedRounds = 0
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
