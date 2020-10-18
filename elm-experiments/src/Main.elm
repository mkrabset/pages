module Main exposing (..)

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Line exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Html exposing (Html,text)
import Html.Attributes exposing (style)
import List exposing (..)
import Dict exposing (..)
import Graphics2D.Matrix exposing (..)
import Graphics2D.Vector exposing (..)
import Graphics2D.Transform exposing (..)

--import Debug exposing (..) 

-- Canvas size
width=800
height=800

circles: Graphics2D.Matrix.Matrix -> Float -> List Canvas.Shape
circles m r = if (r>1)
              then
                [circle (apply m origin) r] 
                ++ (circles (multiply (translation ((r/2),0.0)) m) (r/2))
                ++ (circles (multiply (translation ((-r/2),0.0)) m) (r/2))
--                ++ (circles (multiply (translation (0.0,(r/2))) m) (r/2))
--                ++ (circles (multiply (translation (0.0,(-r/2))) m) (r/2))
              else
                []


scene: List Canvas.Shape -> Canvas.Renderable
scene shapelist = shapes [ transform [translate (width/2) (height/2)]
                            , stroke (Color.rgba 0 0 0 1)
                            , lineWidth 1] 
                            shapelist

-- Creates the view for a given max-value
view = Canvas.toHtml (width, height)
            [ style "border" "none" ]
            ([scene (circles Graphics2D.Matrix.identity (width/2))])

main = view 



