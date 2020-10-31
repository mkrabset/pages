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
import Debug
import Random
import Time

-- Canvas size
width=800
height=800

type alias Circ=
    { x: Float
    , y: Float
    , r: Float
    }

circ x y r = {x=x,y=y,r=r}

type alias Model=
    { growingCircles: List Circ
    , stuckCircles: List Circ
    }

init: ()-> (Model, Cmd Msg)
init _= 
    (
        { growingCircles=[]
        , stuckCircles=[]
        },
        Cmd.none
    )

point : Random.Generator (Float, Float)
point =
  Random.pair (Random.float 0 width) (Random.float 0 height)

type Msg = 
    NeedPoint
    | NewPoint (Float,Float)
    | Tick

growCircle c = {c | r=c.r+1}

grow model = {model | growingCircles=model.growingCircles |> List.map growCircle}

update: Msg -> Model -> (Model,Cmd Msg)
update msg model = case msg of 
    NewPoint (x,y) -> 
        ({model | growingCircles=model.growingCircles++[(circ x y 2)]}, Cmd.none)
    Tick -> 
        (grow model, Cmd.none)
    NeedPoint -> 
        (model, Random.generate NewPoint point)

-- Creates the view for a given max-value
view model= div
            []
            [ div [][]
            , button [onClick NeedPoint][Html.text "Add point"]
            , Canvas.toHtml (width, height)
                []
                [ clear (0,0) width height
                , shapes
                    [transform [translate (0) (0)]
                            , stroke (Color.rgba 0 0 0 1)
                            , lineWidth 1]
                    (model.growingCircles |> List.map (\c->(circle (c.x, c.y) c.r)))
                ]  
            ]

subscriptions model =Time.every 50 (\t->Tick)

main = Browser.element 
    { init=init
    , update=update
    , view=view
    , subscriptions=subscriptions
    }
