module Main exposing (..)

import Browser
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Line exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Html exposing (Html,text, div,button,span, pre)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List exposing (..)
import Array exposing (..)
import Dict exposing (..)
import Debug
import Random
import Time
import Html exposing (a)

-- Canvas size
width=700
height=700

pw=200.0

gcodefactor=pw/width

type alias Circ=
    { x: Float
    , y: Float
    , r: Float
    }

circ x y r = {x=x,y=y,r=r}

type alias Model=
    { growingCircles: List Circ
    , stuckCircles: List Circ
    , tries: Int
    }

collides: Circ -> Circ -> Bool
collides c1 c2 = 
    let
        dx=c1.x-c2.x
        dy=c1.y-c2.y
        rs=c1.r+c2.r+1
    in
        ((dx*dx)+(dy*dy)) <= (rs*rs)

sameCircle: Circ -> Circ -> Bool
sameCircle c1 c2 = (c1==c2) 

edgeCollisions circle = (circle.x-circle.r<1) || (circle.x+circle.r > width) || (circle.y-circle.r<1) || (circle.y+circle.r > height)

anyCollisions: List Circ -> Circ -> Bool
anyCollisions otherCircles circle =  otherCircles 
                                    |> List.filter (\c-> (not (sameCircle c circle)))
                                    |> List.any (\c -> (collides c circle))

init: ()-> (Model, Cmd Msg)
init _= 
    (
        { growingCircles=[]
        , stuckCircles=[]
        , tries=0
        },
        Cmd.none
    )

point : Random.Generator (Float, Float)
point =
  Random.pair (Random.float 0 width) (Random.float 0 height)

type Msg = 
    ClearAll
    | NewPoint (Float,Float)
    | Tick

growCircle c = {c | r=c.r+1}

evalGrow: Model -> Model
evalGrow model = 
                let
                    others=model.growingCircles++model.stuckCircles
                    (a,b)=List.partition (\c->(edgeCollisions c) || (anyCollisions others c)) model.growingCircles
                in
                    { model 
                    | growingCircles=b
                    , stuckCircles=model.stuckCircles++a
                    }


command model = if ((List.length model.growingCircles) < 30 && model.tries<50)
                then Random.generate NewPoint point
                else Cmd.none

grow model = {model | growingCircles=model.growingCircles |> List.map growCircle}

update: Msg -> Model -> (Model,Cmd Msg)
update msg model = case msg of 
    NewPoint (x,y) -> 
        if (anyCollisions (model.growingCircles++model.stuckCircles) (circ x y 0))
        then 
            ({model | tries=model.tries+1}, command model)
        else
            ({model | tries=0, growingCircles=model.growingCircles++[(circ x y 2)]}, Cmd.none)

    Tick -> let
                newGrown=grow (evalGrow model)
            in
                (grow (evalGrow model), command newGrown)

    ClearAll -> init () 


circComp c1 c2 =    if (c1.x<c2.x)  
                        then Basics.LT
                    else if (c1.x>c2.x)
                        then Basics.GT
                    else
                        if (c1.y<c2.y)  
                            then Basics.LT
                        else if (c1.y>c2.y)
                            then Basics.GT
                        else 
                            Basics.EQ
                        

xy: Float -> Float -> String
xy x y = "x"++(String.fromFloat x)++" y"++(String.fromFloat y)

ij: Float -> Float -> String
ij i j = "i"++(String.fromFloat i)++" j"++(String.fromFloat j)

gcode: Circ -> String -> String
gcode c txt =   "m5 \n"++
                "g0 "++(xy ((c.x-c.r)*gcodefactor) (c.y*gcodefactor))++"\n"++
                "m3 \n"++
                "g2 "++(xy ((c.x+c.r)*gcodefactor) (c.y*gcodefactor))++" "++(ij (c.r*gcodefactor) 0)++"\n"++
                "g2 "++(xy ((c.x-c.r)*gcodefactor) (c.y*gcodefactor))++" "++(ij (-(c.r)*gcodefactor) 0)++"\n"++
                "m5 \n"++
                txt

allGcode: Model -> String
allGcode model = case model.growingCircles of
                h::t -> ""
                _ -> List.foldl gcode "" (List.sortWith circComp model.stuckCircles) 


-- Creates the view for a given max-value
view model= div
            []
            [ div [][]
            , button [onClick ClearAll][Html.text "Reset"]
            , Canvas.toHtml (width, height)
                []
                [ clear (0,0) width height
                , shapes
                    [transform [translate (0) (0)]
                            , stroke (Color.rgba 0 0 0 1)
                            , lineWidth 1]
                    ((model.growingCircles++model.stuckCircles) |> List.map (\c->(circle (c.x, c.y) c.r)))
                ]  
            , pre [][Html.text (allGcode model)]
            ]

subscriptions model =Time.every 5 (\t->Tick)

main = Browser.element 
    { init=init
    , update=update
    , view=view
    , subscriptions=subscriptions
    }
