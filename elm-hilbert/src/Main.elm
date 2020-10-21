module Main exposing (..)

import Browser
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Line exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Html exposing (Html,text, div,button,span)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List exposing (..)
import Array exposing (..)
import Dict exposing (..)
import Graphics2D.Matrix exposing (..)
import Graphics2D.Vector exposing (..)
import Graphics2D.Transform exposing (..)

--import Debug exposing (..) 

-- Canvas size
width=800
height=800

forward: Float -> Vector
forward l = (l,0)

type Sym = A | B | P | M | F

leftRotate: Matrix
leftRotate=rotation (pi/2)

rightRotate: Matrix
rightRotate=rotation (-pi/2)

forwardTranslate: Float->Matrix
forwardTranslate l=translation (forward l)

lsystem: Sym -> List Sym
lsystem symbol = case symbol of
                  A -> [M,B,F,P,A,F,A,P,F,B,M]
                  B -> [P,A,F,M,B,F,B,M,F,A,P]
                  x -> [x]

toPath: Float -> Matrix -> Sym -> (Matrix, Array PathSegment)
toPath l m symbol = case symbol of 
                    F -> (multiply m (forwardTranslate l), Array.fromList [lineTo (apply m(forward l))])
                    M -> (multiply m leftRotate, Array.empty)
                    P -> (multiply m rightRotate,Array.empty)
                    _ -> (m, Array.empty)
                    

toSegs: Float -> Int ->  Sym -> (Matrix, Array PathSegment) -> (Matrix, Array PathSegment)
toSegs l depth symbol (m, accum) = if (depth==0)
                                    then
                                        let 
                                            (m2, newsegs)=toPath l m symbol
                                        in
                                            (m2, (Array.append accum newsegs))
                                    else
                                        List.foldl (toSegs l (depth-1)) (m,accum) (lsystem symbol) 

 
scene: List Canvas.Shape -> Canvas.Renderable
scene shapelist = shapes [ transform [translate (10) (10)]
                            , stroke (Color.rgba 0 0 0 1)
                            , lineWidth 1] 
                            shapelist

initMatrix maxDepth = Graphics2D.Matrix.identity

segs: Int -> Array PathSegment
segs maxdepth = case ((toSegs ((width-20)/(2^(toFloat (maxdepth-0)))) maxdepth) A (initMatrix maxdepth, Array.empty)) of
                                         (_,l) -> l

initModel= { depth=1
           , renderable= scene [path (0,0) (Array.toList (segs 1))]
           }

type Msg = Up | Down

update msg model = let
                    newDepth=case msg of
                                Up -> min 9 (model.depth+1)
                                Down -> max 0 (model.depth-1)
                    in 
                        if (newDepth==model.depth)
                        then model
                        else
                            { depth=newDepth
                            , renderable=scene [path (0,0) (Array.toList (segs newDepth))]
                            }


-- Creates the view for a given max-value
view model= div
            []
            [ button[onClick Down][Html.text "-"]
            , span [][Html.text (String.fromInt model.depth)] 
            , button[onClick Up][Html.text "+"]
            ,div [][]
            , Canvas.toHtml (width, height)
                [ style "border" "none" ]
                [ clear (0,0) width height
                , model.renderable
                ]
                
            ]

main = Browser.sandbox 
    { init=initModel
    , update=update
    , view=view 
    }
