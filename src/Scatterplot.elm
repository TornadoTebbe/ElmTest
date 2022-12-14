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
import Csv exposing (Csv)
import TypedSvg.Attributes exposing (points)

--Definition für Daten einlesen

type Model
    = Loading
    | Data Student_Data
    | Failure
    | Success2 String




type alias Student_Data =
    {   certification : String
    ,   gender : String
    ,   department : String
    ,   height : Float
    ,   weight : Float
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
        (Csv.Decode.field "Certification Course" Ok
              |> Csv.Decode.andMap (Csv.Decode.field "Gender" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "Department" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "Height(CM)"(String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Weight(KG)" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "10th Mark" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "12th Mark" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "college mark" (String.toFloat >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "hobbies" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "daily studing time" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "prefer to study in" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "salary expectation" (String.toInt >> Result.fromMaybe "error parsing string"))
              |> Csv.Decode.andMap (Csv.Decode.field "Do you like your degree?" Ok) --(String.toBool >> Result.fromMaybe "error parsing string")) 
              |> Csv.Decode.andMap (Csv.Decode.field "willingness to pursue a career based on their degree  " Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "social medai & video" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "Travelling Time " Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "Stress Level " Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "Financial Status" Ok)
              |> Csv.Decode.andMap (Csv.Decode.field "part-time job" Ok) --(String.tool >> Result.fromMaybe "error parsing string")) 
        )

studentListe : List String -> List Student_Data
studentListe student_liste =
    List.map(\x -> csvString_to_data x) student_liste
        |> List.concat



--hochladen der Daten aus dem Github
init : () -> ( Model, Cmd Msg )
init _ =
    (Loading , 
    Http.get
    { url = "https://raw.githubusercontent.com/TornadoTebbe/ElmTest/main/Daten/Student_Behaviour.csv"
    , expect = Http.expectString GotText
    })



--Scatterplot Definitionen

type alias Point =
    {   pointDescription : String
    ,   salaryExpectation : Int
    ,   tenthMark  : Float
    ,   twelthMark  : Float
    ,   collegeMark : Float
    ,   dailyStudyingTime : String
    ,   preferStudyTime : String
    ,   satisfyDegree : String --Bool
    ,   socialMedia : String
    ,   stressLevel : String
    ,   financialStatus : String
    ,   partTimeJob : String --Bool
    }


type alias XyData =
    { xDescription : String
    , yDescription : String
    , data : List Point
    }



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


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )



wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        exvals =
            case Statistics.extent values of
                Just vals ->
                    vals

                _ ->
                    defaultExtent

        len =
            Tuple.second exvals - Tuple.first exvals

        extension =
            0.1 * len

        limits =
            ( max 0 (Tuple.first exvals - extension), Tuple.second exvals + extension )
    in
    limits


xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) (wideExtent values)


yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) (wideExtent values)


xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)


yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)


drawPoint : ContinuousScale Float -> ContinuousScale Float -> Point ->(Float, Float) -> Svg msg
drawPoint scalex scaley point scatterPoint =
        g [ class [ "point" ], fontSize <| Px 10.0, fontFamily [ "sans-serif" ] ]       
                [ circle
                    [ cx (Scale.convert scalex (Tuple.first scatterPoint))
                    , cy (Scale.convert scaley (Tuple.second scatterPoint))
                    , r radius               
                    ]
                    [] 
                    , text_
                    [ x (Scale.convert scalex (Tuple.first scatterPoint))
                    , y (Scale.convert scaley (Tuple.second scatterPoint) - (radius + 3))
                    , textAnchor AnchorMiddle
                    ]
                    [Html.text point.pointDescription]
                ]


scatterplot : XyData -> List Float -> List Float -> String -> String -> Svg msg
scatterplot model xValues yValues xDescription yDescription =
    let

        
        scatterPoint =
            List.map2 (\x y -> ( x, y )) xValues yValues

        xSkalierung : ContinuousScale Float
        xSkalierung =
            xScale xValues

        ySkalierung : ContinuousScale Float
        ySkalierung =
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
            [ transform
                [ Translate (padding - 1) (padding - 1 + Tuple.first (Scale.range ySkalierung))
                ]
            ]
            [ xAxis xValues
            , text_
                [ x (Scale.convert xSkalierung labelPositions.x)
                , y 30
                , textAnchor AnchorMiddle
                , fontSize <| Px 10.0
                , fontFamily [ "sans-serif" ]
                ]
                [ text model.xDescription ]
            ]

        , g [ transform [ Translate (padding - 1) (padding - 1) ] ]
            [ yAxis yValues
            , text_
                [ y (Scale.convert ySkalierung labelPositions.y - (1 / 3 * padding))
                , x 0
                , textAnchor AnchorMiddle
                , fontSize <| Px 10.0
                , fontFamily [ "sans-serif" ]
                ]
                [ text model.yDescription ]
            ]

        , g 
             [transform [ Translate padding padding ] ]
                (List.map2 (drawPoint xSkalierung ySkalierung) model.data scatterPoint)
        ]




--Bilden Map funktion für Filtern der Daten

andMap : Maybe a -> Maybe (a -> b) -> Maybe b
andMap =
  Maybe.map2 (|>)
  
map11 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l) 
  -> Maybe a
  -> Maybe b
  -> Maybe c
  -> Maybe d
  -> Maybe e
  -> Maybe f
  -> Maybe g
  -> Maybe h
  -> Maybe i
  -> Maybe j
  -> Maybe k
  -> Maybe l


map11 function maybe1 maybe2 maybe3 maybe4 maybe5 maybe6 maybe7 maybe8 maybe9 maybe10 maybe11 =
  Just function
    |> andMap maybe1
    |> andMap maybe2
    |> andMap maybe3
    |> andMap maybe4
    |> andMap maybe5
    |> andMap maybe6
    |> andMap maybe7
    |> andMap maybe8
    |> andMap maybe9
    |> andMap maybe10
    |> andMap maybe11





studenttoPoint : Student_Data -> Maybe Point
studenttoPoint student =
    map11
        (\salaryExpectation tenthMark twelthMark collegeMark dailyStudyingTime preferStudyTime satisfyDegree socialMedia stressLevel financialStatus partTimeJob  ->
            Point
                (student.gender ++ " (" ++ String.fromFloat student.height ++ "," ++ String.fromFloat student.weight ++ ")")
                (salaryExpectation)
                (tenthMark)
                (twelthMark)
                (collegeMark)
                (dailyStudyingTime)
                (preferStudyTime)
                (satisfyDegree)
                (socialMedia)
                (stressLevel)
                (financialStatus)
                (partTimeJob)
        )
        
        (Just student.salaryExpectation)
        (Just student.tenthMark)
        (Just student.twelthMark)
        (Just student.collegeMark)
        (Just student.dailyStudyingTime)
        (Just student.preferStudyTime)
        (Just student.satisfyDegree)
        (Just student.socialMedia)
        (Just student.stressLevel)
        (Just student.financialStatus)
        (Just student.partTimeJob)







filterAndReduceStudents : List Student_Data -> XyData
filterAndReduceStudents students =
    let
        filter =
            List.filterMap studenttoPoint students
    in
    XyData "salary expectation" "college mark" filter


stringToCsv : String -> Csv
stringToCsv rawData =
    Csv.parse rawData


{--
--Aufbereitung der Daten für den View
dataForView : Student_Data -> Html Msg
dataForView studenBehavData =
    let
        csv = 
            stringToCsv sBData.dataAsStr
        
        filteredAndReduceStudents= filterAndReduceStudentssBData

    in

    --}



--Displaying csv
tableHeaderItem : String -> Html Msg
tableHeaderItem content =
    Html.th [] [ Html.text content ]

tableRowItem : List String -> Html Msg
tableRowItem content =
    Html.tr [] (List.map cellItem content)

cellItem : String -> Html Msg
cellItem content =
    td [] [ Html.text content ]



viewDataToString : String -> Html Msg
viewDataToString data =
    let
        csv = 
            stringToCsv data
    in
    
    div []
        [ h1 [] [ Html.text "Student Behaviour" ]
        --, div [] [ text data ]
        , div []
            [ Html.table []
                [ Html.thead [] [ Html.tr [] (List.map tableHeaderItem csv.headers) ]
                , Html.tbody [] (List.map tableRowItem csv.records)
                ]
            ]
        ]





update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                   {--( { model | status = Success2, data = model.data ++ fullText }, Cmd.none )--} 
                   (Success2 fullText, Cmd.none)

                Err _ ->
                  {--( { model | status = Failure }, Cmd.none )--}  
                  (Failure, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "I was unable to load your book."

        Loading ->
            text "Loading..."

        Data studenBehavData ->
           dataForView studenBehavData




        {--Success2 fullText->
            Html.div []
                [ Html.p []
                    [
                         text (String.fromInt (List.length (studentListe [fullText])))
                    ]
                , scatterplot 
                ]
                --}
        
            
            {--pre [] [ text (String.fromInt (List.length (studentListe [fullText]))) ] --}


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

