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
width=1200
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

branches: Graphics2D.Matrix.Matrix -> Float -> Float -> List Canvas.Shape
branches m l depth =  if (l > 1)
                then
                    let
                        start = (apply m (0,0))
                        end = (apply m (l,0))
                        translated = multiply m (translation (l,0))
                        translatedAndRotatedLeft = (multiply translated (rotation (pi/4.1)))
                        translatedAndRotatedRight = (multiply translated (rotation (-pi/5)))
                    in 
                        [path (0,0) 
                            [moveTo start, lineTo end]
                        ] 
                        ++ (branches translatedAndRotatedLeft (l*0.75) (depth+1))
                        ++ (branches translatedAndRotatedRight (l*0.60) (depth+1))
                else 
                    []


scene: List Canvas.Shape -> Canvas.Renderable
scene shapelist = shapes [ transform [translate (width/3) (height), rotate (-pi/2)]
                            , stroke (Color.rgba 0 0 0 1)
                            , lineWidth 1] 
                            shapelist

-- Creates the view for a given max-value
view = Canvas.toHtml (width, height)
            [ style "border" "none" ]
            ([scene (branches Graphics2D.Matrix.identity (width/5) 0)])

main = view 



