module Main exposing (..)

import Browser
import Html exposing (div, text, input, button)
import Html.Events exposing (onClick, onInput)
import String exposing (fromInt, toInt)
import Debug exposing (log)


type Messages =
    Add
    | ChangedAddText String

init = 
    { value = 50
    , firstName = "Paul"
    , numberToAdd = 0}

view model =
    div [] [
        text (fromInt model.value)
        , div [][]
        , input [ onInput ChangedAddText][]
        , button [onClick Add][text "Add"]]


parseUserNumber
update msg model =
    let
        log1 = log "message" msg
        log2 = log "model" model
    in

    case msg of
        Add -> 
            {model | value = 70}
        ChangedAddText theText->
            { model | numberToAdd = toInt theText }




main =
    Browser.sandbox
        {
            init = init
            , view = view
            , update = update
        }