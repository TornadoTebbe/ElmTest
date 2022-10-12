module Main exposing (..)

import Browser
import Html exposing (div, text, input, button)
import Html.Events exposing (onClick)
import String exposing (fromInt)
import Debug exposing (log)


type Messages =
    Add
    | Other
    | Chicken

init = 
    { value = 54}

view model =
    div [] [
        text (fromInt model.value)
        , div [][]
        , input [][]
        , button [onClick Add][text "Add"]]

update msg model =
    let
        a = 1
        b = 2
        logmessage = log "yo"
    in
    



    case msg of
        Add -> 
            model

        Other -> 
            model

        Chicken -> 
            model



main =
    Browser.sandbox
        {
            init = init
            , view = view
            , update = update
        }