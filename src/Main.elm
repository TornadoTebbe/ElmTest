module Main exposing (..)

import Browser
import Html exposing (div, text, input, button)
import Html.Events exposing (onClick)
import String exposing (fromInt)
import Debug exposing (log)


type Messages =
    Add

init = 
    { value = 50, firstName = "Paul"}

view model =
    div [] [
        text (fromInt model.value)
        , div [][]
        , input [][]
        , button [onClick Add][text "Add"]]

update msg model =
    let
        log1 = log "message" msg
        log2 = log "model" model
    in

    case msg of
        Add -> 
            {model | value = 70}



main =
    Browser.sandbox
        {
            init = init
            , view = view
            , update = update
        }