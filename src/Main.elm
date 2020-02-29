module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html)
import String
import Time



-- Model


type Status
    = Init
    | InProgress
    | Finished


type alias Model =
    { status : Status
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
            case model.status of
                InProgress ->
                    ( { model | totalTime = model.totalTime + 1 }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StartChallenge ->
            ( { model | status = InProgress }, Cmd.none )

        RoundDone ->
            ( model, Cmd.none )



-- View


view : Model -> Html msg
view model =
    model.totalTime
        |> String.fromInt
        |> Html.text



-- Init


init : () -> ( Model, Cmd Msg )
init _ =
    ( { status = Init
      , finishedRounds = 0
      , totalTime = 0
      , remainingRest = 0
      , maxReps = 10
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
