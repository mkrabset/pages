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

type Sym = A | B | P | M

leftRotate: Matrix
leftRotate=rotation (pi/3)

rightRotate: Matrix
rightRotate=rotation (-pi/3)

forwardTranslate: Float->Matrix
forwardTranslate l=translation (forward l)

lsystem: Sym -> List Sym
lsystem symbol = case symbol of
                  A -> [B,M,A,M,B]
                  B -> [A,P,B,P,A]
                  x -> [x]

toPath: Float -> Matrix -> Sym -> (Matrix, List PathSegment)
toPath l m symbol = case symbol of 
                    A -> (multiply m (forwardTranslate l), [lineTo (apply m(forward l))])
                    B -> (multiply m (forwardTranslate l), [lineTo (apply m(forward l))])
                    M -> (multiply m leftRotate,[])
                    P -> (multiply m rightRotate,[])
--                    _ -> (m,[])
                    



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

initMatrix maxDepth = if ((modBy 2 maxDepth)==1) 
                        then 
                            Graphics2D.Matrix.identity
                        else
                            leftRotate

segs: Int -> List PathSegment
segs maxdepth = case ((toSegs ((width-20)/(2^(toFloat (maxdepth-0)))) maxdepth) A (initMatrix maxdepth, [])) of
                                         (_,l) -> l

initModel= { depth=0
           , renderable= scene [path (0,0) (segs 0)]
           }

type Msg = Up | Down

update msg model = let
                    newDepth=case msg of
                                Up -> min 8 (model.depth+1)
                                Down -> max 0 (model.depth-1)
                    in 
                        if (newDepth==model.depth)
                        then model
                        else
                            { depth=newDepth
                            , renderable=scene [path (0,0) (segs newDepth)]
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
