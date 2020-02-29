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
    , round : Int
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
    { model
        | totalTime = model.totalTime + 1
    }


maxRound : Model -> Int
maxRound model =
    2 * model.maxReps - 1


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
        newModel =
            { model
                | round = model.round + 1
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
            newModel.round <= maxRound newModel
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
      , round = 0
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
