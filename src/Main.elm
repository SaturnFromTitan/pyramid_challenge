module Main exposing (main)

import Browser
import Model exposing (Model)
import Time
import Update exposing (Msg(..), subscriptions, update)
import View exposing (view)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model.initialModel
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.element
        { update = Update.update
        , view = View.view
        , init = init
        , subscriptions = Update.subscriptions
        }
