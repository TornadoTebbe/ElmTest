module Main exposing (..)

import Browser
import Html exposing (div, text, input, button)
import Html.Events exposing (onClick)
import String exposing (fromInt)
import Debug exposing (log)


type Messages =
    Add


init = 
    { value = 54}

view model =
    div [] [
        text (fromInt model.value)
        , div [][]
        , input [][]
        , button [onClick Add][text "Hinzufügen für Bonus"]]

update msg model =
    let
        log1 = log "msg" msg
        log2 = log "model" model


    in
    



    case msg of
        Add -> 
            model




main =
    Browser.sandbox
        {
            init = init
            , view = view
            , update = update
        }