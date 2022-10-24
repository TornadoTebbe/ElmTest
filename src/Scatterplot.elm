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


type alias XyData =
    { xDescription : String
    , yDescription : String
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
{--
        xyPoints =
            List.map2 (\x y -> ( x, y )) xValues yValues
          --}
          
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
