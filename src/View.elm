module View exposing (view)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Progress as Progress
import Bootstrap.Text as Text
import Bootstrap.Utilities.Spacing as Spacing
import Html exposing (Html)
import Html.Attributes as Attributes
import Model exposing (Model, getGameText, getReps, getTotalReps, maxTotalReps)
import Update exposing (Msg(..))
import Utilities exposing (secondsToTime)


view : Model -> Html Msg
view model =
    let
        elements =
            case model.status of
                Model.Init ->
                    [ gameTextRow model
                    , buttonsRow model
                    ]

                Model.Pushing ->
                    [ gameTextRow model
                    , progressRow model
                    , nextRepsRow model
                    , buttonsRow model
                    ]

                Model.Resting ->
                    [ gameTextRow model
                    , progressRow model
                    , restRow model
                    ]

                Model.Finished ->
                    [ gameTextRow model
                    , totalTimeRow model
                    ]
    in
    Html.div []
        [ CDN.stylesheet
        , Grid.container [] elements
        ]


makeDefaultRow : List (Html msg) -> List (Html.Attribute msg) -> Html msg
makeDefaultRow elements rowAttributes =
    Grid.row
        [ Row.centerXs, Row.attrs rowAttributes ]
        [ Grid.col
            [ Col.xs5, Col.textAlign Text.alignXsCenter ]
            elements
        ]


gameTextRow : Model -> Html msg
gameTextRow model =
    makeDefaultRow
        [ Html.h3 [] [ Html.text (getGameText model) ] ]
        [ Spacing.p1, Spacing.pt5, Spacing.mt5 ]


progressRow : Model -> Html msg
progressRow model =
    let
        finishedReps =
            toFloat (getTotalReps model.finishedRounds)

        progress =
            Progress.progress
                [ Progress.success
                , Progress.value finishedReps
                ]

        progressText =
            String.concat
                [ String.fromInt (getTotalReps model.finishedRounds)
                , " out of "
                , String.fromInt maxTotalReps
                ]
    in
    makeDefaultRow
        [ progress
        , Html.text progressText
        ]
        [ Spacing.p1 ]


restRow : Model -> Html msg
restRow model =
    makeDefaultRow
        [ Html.h2 [] [ Html.text (secondsToTime model.remainingRest) ] ]
        [ Spacing.pt5, Spacing.pb4 ]


nextRepsRow : Model -> Html msg
nextRepsRow model =
    let
        message =
            String.concat
                [ String.fromInt (getReps (model.finishedRounds + 1))
                ]
    in
    makeDefaultRow
        [ Html.text "Now do"
        , Html.h1 [] [ Html.text message ]
        ]
        [ Spacing.pt5, Spacing.pb4 ]


totalTimeRow : Model -> Html msg
totalTimeRow model =
    let
        message =
            String.concat
                [ "Your time: "
                , secondsToTime model.totalTime
                ]
    in
    makeDefaultRow [ Html.text message ] [ Spacing.p1 ]


buttonsRow : Model -> Html Msg
buttonsRow model =
    let
        buttons =
            case model.status of
                Model.Init ->
                    [ Button.submitButton [ Button.primary, Button.onClick StartChallenge ] [ Html.text "Let's go" ] ]

                Model.Pushing ->
                    [ Button.submitButton [ Button.primary, Button.onClick RoundDone ] [ Html.text "Done" ] ]

                _ ->
                    []
    in
    makeDefaultRow buttons [ Spacing.p1 ]