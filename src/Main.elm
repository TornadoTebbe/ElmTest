module Main exposing (..)

import Browser
import Html exposing (div, text)


add a b = a + b

init = 
    { value = 0}

view model =
    div [] [text "Yo"]

update model =
    model



main =
    Browser.sandbox
        {
            init = init
            , view = view
            , update = update
        }