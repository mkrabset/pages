module Main exposing (..)

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Line exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Html exposing (Html,text, div)
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

forward: Float -> Vector
forward l = (l,0)

type Sym = F | G | P | M | I

leftRotate: Matrix
leftRotate=rotation (pi*2/3)

rightRotate: Matrix
rightRotate=rotation (-pi*2/3)

forwardTranslate: Float->Matrix
forwardTranslate l=translation (forward l)

lsystem: Sym -> List Sym
lsystem symbol = case symbol of
                  I -> [F,M,G,M,G]
                  F -> [F,M,G,P,F,P,G,M,F]
                  G -> [G,G]
                  x -> [x]

toPath: Float -> Matrix -> Sym -> (Matrix, List PathSegment)
toPath l m symbol = case symbol of 
                    F -> (multiply m (forwardTranslate l), [lineTo (apply m(forward l))])
                    G -> (multiply m (forwardTranslate l), [lineTo (apply m(forward l))])
                    M -> (multiply m leftRotate,[])
                    P -> (multiply m rightRotate,[])
                    _ -> (m,[])
                    



toSegs: Float -> Int ->  Sym -> (Matrix, List PathSegment) -> (Matrix, List PathSegment)
toSegs l depth symbol (m, accum) = if (depth==0)
                                    then
                                        let 
                                            (m2, newsegs)=toPath l m symbol
                                        in
                                            (m2, accum++newsegs)
                                    else
                                        List.foldl (toSegs l (depth-1)) (m,accum) (lsystem symbol) 

 
scene: List Canvas.Shape -> Canvas.Renderable
scene shapelist = shapes [ transform [translate (10) (10)]
                            , stroke (Color.rgba 0 0 0 1)
                            , lineWidth 1] 
                            shapelist

initMatrix=Graphics2D.Matrix.identity

segs: Int -> List PathSegment
segs maxdepth = case ((toSegs ((width-20)/(2^(toFloat (maxdepth-1)))) maxdepth) I (initMatrix, [])) of
                                         (_,l) -> l



-- Creates the view for a given max-value
view maxdepth= div[][Canvas.toHtml (width, height)
            [ style "border" "none" ]
            ([scene [path (0,0) (segs maxdepth)]])]

main = view 8



