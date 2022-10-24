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
