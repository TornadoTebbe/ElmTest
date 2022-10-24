module Main exposing (..)

import Browser
import Csv
import Csv.Decode
import Html exposing (div, text, input, button, pre, Html)
import Html.Events exposing (onClick, onInput)
import String exposing (fromInt, toInt)
import Debug exposing (log)
import Http


type Status
    = Success
    | Loading
    | Failure


type alias Model =
    { status : Status
    , data : String
    }


type Msg
    = GotText (Result Http.Error String)


init : () -> ( Model, Cmd Msg )
init _ =
    let
        data =
            [ "Student_Behaviour"]
        cmds =
            List.map
                (\x ->
                    Http.get
                        { url = "http://localhost:8000/Daten/" ++ x ++ ".csv"
                        , expect = Http.expectString GotText
                        }
                )
                data
    in
    ( { status = Loading, data = "" }
    , Cmd.batch cmds
    )

decodeStockDay : Csv.Decode.Decoder (( String, Maybe Float ) -> a) a
decodeStockDay =
    Csv.Decode.map
        (\gender twelthMark ->
            ( gender
            , case String.toFloat twelthMark of
                Just o ->
                    Just o

                Nothing ->
                    Nothing
            )
        )
        (Csv.Decode.andMap
            (Csv.Decode.field "12th Mark" Ok)
            (Csv.Decode.field "Gender" Ok)
        )


csvString_to_data : String -> List ( String, Maybe Float )
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeStockDay
        |> Result.toMaybe
        |> Maybe.withDefault []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( { model | status = Success, data = model.data ++ fullText }, Cmd.none )

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    case model.status of
        Failure ->
            text "I was unable to load your book."

        Loading ->
            text "Loading..."

        Success ->
            pre [] [ text model.data ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
