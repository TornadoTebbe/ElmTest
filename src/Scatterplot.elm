module Scatterplot exposing (..)

import Axis
import Html exposing (..)
import Html.Attributes exposing (value, href)
import Html.Events exposing (onInput)
import Scale exposing (ContinuousScale)
import Shape exposing (..)
import Statistics
import TypedSvg exposing (circle, g, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))
import Browser
import Http
import Csv.Decode exposing (..)
import Csv exposing (..)
import TypedSvg.Attributes exposing (points)


{--type alias XyData =
    { xDescription : String
    , yDescription : String
    }  
    
--}

type Status
    = Success
    | Loading
    | Failure


type alias Model =
    { status : Status
    , data : String
    }


type alias Student_Data =
    {    gender : String
    ,   department : String
    ,   height : Int
    ,   weight : Int
    ,   tenthMark  : Float
    ,   twelthMark  : Float
    ,   collegeMark : Float
    ,   hobbies : String
    ,   dailyStudyingTime : String
    ,   preferStudyTime : String
    ,   salaryExpectation : Int
    ,   satisfyDegree : String --Bool
    ,   willignessDegree: String
    ,   socialMedia : String
    ,   travellingTime : String
    ,   stressLevel : String
    ,   financialStatus : String
    ,   partTimeJob : String --Bool
    }

type Msg
  = GotText (Result Http.Error String)




--Einlesen der Daten

daten: List String
daten = ["Student_Behaviour.csv"]

{--
csvDatenLaden: (Result Http.Error String -> Msg) -> Cmd Msg
csvDatenLaden msg =
     daten 
    |> List.map 
      (\d ->
      Http.get
      { url = "https://raw.githubusercontent.com/nikschwa/Projekt-Information-Retrieval-und-Visualisierung/main/" ++ d
      , expect = Http.expectString msg
      }    
    )
    |> Cmd.batch
  
--}

--Decodierung der Daten:

csvString_to_data : String -> List Student_Data
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeCsvStudentdata
        |> Result.toMaybe
        |> Maybe.withDefault []


decodeCsvStudentdata : Csv.Decode.Decoder (Student_Data -> a ) a 
decodeCsvStudentdata =
    Csv.Decode.map Student_Data
        (Csv.Decode.field "Gender" Ok
              |> Csv.Decode.andMap (Csv.Decode.field "Department" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "Height(CM)"(String.toInt >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Weight(KG)" (String.toInt >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "10th Mark" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "12th Mark" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "college mark" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "hobbies" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "daily studying time" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "prefer to study in" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "salary expectation" (String.toInt >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Do you like your Degree?" Ok) --(String.toBool >> Result.fromMaybe "error parsing string")) 
              |> Csv.Decode.andMap (Csv.Decode.field "willingness to pursue a career based on their degree" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "social medai & video" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "Travelling Time" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "Stress Level" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "Financial Status" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "part time job" Ok) --(String.tool >> Result.fromMaybe "error parsing string")) 
        )

studentListe :List String -> List Student_Data
studentListe student_liste =
    List.map(\x -> csvString_to_data x) student_liste
        |> List.concat



--hochladen der Daten aus dem Github
init : () -> ( Model, Cmd Msg )
init _ =
    let
        data =
            [ "Student_Behaviour"]
        cmds =
            List.map
                (\x ->
                    Http.get
                        { url = "https://raw.githubusercontent.com/TornadoTebbe/ElmTest/main/Daten/Student_Behaviour.csv"
                        , expect = Http.expectString GotText
                        }
                )
                data
    in
    ( { status = Loading, data = "" }
    , Cmd.batch cmds
    )







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







{--
--Erstellung Scatterplot

w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    60


radius : Float
radius =
    5.0


tickCount : Int
tickCount =
    5


xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)


yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)


xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) (wideExtent values)


yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) (wideExtent values)


wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        closeExtent =
            Statistics.extent values
                |> Maybe.withDefault defaultExtent

        extension =
            (Tuple.second closeExtent - Tuple.first closeExtent) / toFloat (2 * tickCount)
    in
    ( Tuple.first closeExtent - extension |> max 0
    , Tuple.second closeExtent + extension
    )


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )



scatterplot : Svg msg
scatterplot =
    let

        xyPoints =
            List.map2 (\x y -> ( x, y )) xValues yValues
          
          
        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale yValues

        half : ( Float, Float ) -> Float
        half t =
            (Tuple.second t - Tuple.first t) / 2

        labelPositions : { x : Float, y : Float }
        labelPositions =
            { x = wideExtent xValues |> half
            , y = wideExtent yValues |> Tuple.second
            }
                         
              
    in            
    
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(255, 255, 255,0.3); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
          """ ]
        , g
            [ transform [ Translate (padding - 1) ( padding - 1 ) ]
            , class [ "point" ]
            , fontSize <| Px 10.0
            , fontFamily [ "sans-serif" ]
            ]
            []
            
        , g 
            [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis xValues
             , text_
                [ x (Scale.convert xScaleLocal labelPositions.x)
                 , y 30                
                ]
                [ text xLabel ]
            ]
        , g 
            [transform [ Translate (padding - 1) padding ] ]
            [ yAxis yValues                             
            , text_
                [ x -40
                , y ( Scale.convert yScaleLocal labelPositions.y - 15)  
                ]
                [ text yLabel ]         
            ]
         , g 
             [transform [ Translate padding padding ] ]
                (List.map2 (point xScaleLocal yScaleLocal) model.data xyPoints)
              
        ]


--}