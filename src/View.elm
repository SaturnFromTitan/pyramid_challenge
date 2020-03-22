module View exposing (view)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Radio as Radio
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Progress as Progress
import Bootstrap.Text as Text
import Bootstrap.Utilities.Spacing as Spacing
import Html exposing (Html)
import Html.Attributes as Attributes
import Model exposing (Model, getGameText, getRepsOfRound, getTotalFinishedReps, getTotalReps)
import Update exposing (Msg(..))
import Utilities exposing (secondsToTime)


view : Model -> Html Msg
view model =
    let
        elements =
            case model.status of
                Model.Init ->
                    [ gameTextRow model
                    , inputFormRow
                    ]

                Model.Doing ->
                    [ gameTextRow model
                    , progressRow model
                    , nextRepsRow model
                    , roundDoneButtonRow
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


gameTextRow : Model -> Html msg
gameTextRow model =
    makeDefaultRow
        [ Html.h3 [] [ Html.text (getGameText model) ] ]
        [ Spacing.p1, Spacing.pt5, Spacing.mt5 ]


inputFormRow : Html Msg
inputFormRow =
    makeDefaultRow
        [ Form.form []
            [ exerciseRadioButtons
            , startGameButton
            ]
        ]
        [ Spacing.p1 ]


exerciseRadioButtons : Html Msg
exerciseRadioButtons =
    Form.group []
        (Radio.radioList "exercise-radios"
            [ Radio.create [ Radio.inline, Radio.onClick (SetExercise Model.Pushups), Radio.checked True ] "Pushups"
            , Radio.create [ Radio.inline, Radio.onClick (SetExercise Model.Pullups) ] "Pullups"
            ]
        )


startGameButton : Html Msg
startGameButton =
    makeSubmitButton StartChallenge "Let's go!"


progressRow : Model -> Html msg
progressRow model =
    let
        finishedReps =
            toFloat (getTotalReps model)

        maxTotalReps =
            toFloat (getTotalFinishedReps model)

        valueAsPercent =
            finishedReps / maxTotalReps * 100

        progress =
            Progress.progress
                [ Progress.success
                , Progress.value valueAsPercent
                ]

        progressText =
            String.concat
                [ String.fromInt (getTotalReps model)
                , " out of "
                , String.fromInt (getTotalFinishedReps model)
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
                [ String.fromInt (getRepsOfRound model (model.finishedRounds + 1))
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


roundDoneButtonRow : Html Msg
roundDoneButtonRow =
    let
        button =
            makeSubmitButton RoundDone "Done"
    in
    makeDefaultRow [ button ] [ Spacing.p1 ]


makeDefaultRow : List (Html msg) -> List (Html.Attribute msg) -> Html msg
makeDefaultRow elements rowAttributes =
    Grid.row
        [ Row.centerXs, Row.attrs rowAttributes ]
        [ Grid.col
            [ Col.xs5, Col.textAlign Text.alignXsCenter ]
            elements
        ]


makeSubmitButton : Msg -> String -> Html Msg
makeSubmitButton msg text =
    Button.submitButton
        [ Button.primary
        , Button.onClick msg
        , Button.attrs [ Spacing.ml2, Spacing.mr2 ]
        ]
        [ Html.text text ]
