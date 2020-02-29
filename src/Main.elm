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
    { model | totalTime = model.totalTime + 1 }


startChallenge : Model -> Model
startChallenge model =
    { model
        | gameStatus = InProgress
        , roundStatus = Pushing
    }


advanceRound : Model -> Model
advanceRound model =
    { model | finishedRounds = model.finishedRounds + 1 }



-- View


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.text (Debug.toString model)
        , Html.br [] []
        , Html.button [ onClick RoundDone ] [ Html.text "Round Done!" ]
        , Html.button [ onClick StartChallenge ] [ Html.text "Start Challenge!" ]
        ]



--
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
