module Main exposing (..)

import Browser
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Line exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Html exposing (Html,text, div,button,span, pre)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick,on)
import List exposing (..)
import Array exposing (..)
import Dict exposing (..)
import Debug
import Random 
import Time
import Html exposing (a)
import Json.Decode exposing (Decoder, int, at, map)
import QuadTree.Bounds exposing (Bounds)
import QuadTree.QuadTree
import QuadTree.Vector2d exposing (Vector2d,multiply,add,toPair,fromPair,sqLength,subtract)
import QuadTree.Renderables exposing (gridShapes, bubbleShapes)
import QuadTree.Bubble exposing (Bubble)

-- Time delta for each step
timeDeltaMillis=10

-- Maximum initial axial bubble speed
maxSpeed = 100

-- Canvas size
width=1200
height=700

-- Number of bubbles added for each mouse-click
bubblesAddedForEachClick=50

-- Mouse click event data (and decoder)
type alias MouseDownData = 
    { offsetX : Int
    , offsetY : Int
    }

decoder : Decoder MouseDownData
decoder =
    Json.Decode.map2 MouseDownData
        (at [ "offsetX" ] int)
        (at [ "offsetY" ] int)

-- Model consisting of a list of bubbles
type alias Model= 
    { bubbles: List Bubble
    }

-- Initial model: empty bubble list and no command
init: ()-> (Model, Cmd Msg)
init _= ( {bubbles=[]}
        , newRandomBubbleCommand 10
        )

-- Generator for random position and velocity
randomPosVel: Random.Generator ( ( Float, Float ), ( Float, Float ) )
randomPosVel = Random.pair (Random.pair (Random.float 0 width) (Random.float 0 height)) (Random.pair (Random.float -maxSpeed maxSpeed) (Random.float -maxSpeed maxSpeed))

-- Msg type
type Msg = 
    ClearAll
    | NewBubble Int ((Float,Float),(Float,Float))
    | Tick
    | MouseDown MouseDownData

-- Command for generating a number of new bubbles
newRandomBubbleCommand num = Random.generate (NewBubble num) randomPosVel

-- Applies modulo on number, used for keeping the bubbles on-screen
modval mod v = if (v>mod) then v-mod
                else if (v<0) then v+mod
                else v

-- Updates bubble position
updateBubble: Bubble -> Bubble
updateBubble bubble = 
    let 
        delta=multiply (timeDeltaMillis/1000.0) bubble.vel
        newPos=add bubble.pos delta
        (x,y)=(modval width newPos.x, modval height newPos.y)
    in
        {bubble | pos={x = x, y = y}}


-- Main model update function
update: Msg -> Model -> (Model,Cmd Msg)
update msg model = case msg of 
    ClearAll -> init () 

    Tick -> 
        ({model | bubbles=(model.bubbles |> List.map updateBubble)}, Cmd.none)

    NewBubble num (pos,vel) ->
        if (num>0) then
            let
                newBubble = { pos = (fromPair pos), vel=(fromPair vel), radius=5}
            in
                ({model | bubbles=newBubble::model.bubbles}, newRandomBubbleCommand (num-1))
        else 
            (model, Cmd.none)
    
    MouseDown data -> (model, newRandomBubbleCommand bubblesAddedForEachClick)


-- Create bounds for a bubble
bubbleBounds: Bubble -> Bounds
bubbleBounds b = 
    { center = b.pos
    , rx = b.radius
    , ry = b.radius
    }


-- Wrap bubble in Shape
toShape: Bubble -> QuadTree.QuadTree.Shape Bubble
toShape bubble = { bounds = (bubbleBounds bubble), data=bubble}


bubbleCollision b1 b2 = 
    let
        maxDistSq=(b1.radius + b2.radius)*(b1.radius + b2.radius)
        distSq=sqLength (subtract b1.pos b2.pos)
    in
        distSq<=maxDistSq

anyCollision: QuadTree.QuadTree.Node Bubble -> QuadTree.QuadTree.Shape Bubble -> Bool                
anyCollision tree bubbleShape = QuadTree.QuadTree.collisions tree bubbleShape 
                                |> List.filter (\s -> s.data/=bubbleShape.data)
                                |> List.any (\s -> bubbleCollision bubbleShape.data s.data)

-- Creates the view for a given max-value
view model= 
    let
        bShapes=(model.bubbles |> List.map toShape)
        tree=QuadTree.QuadTree.create 3 bShapes

        collisionTest=case tree of 
            Nothing-> (\_->False)
            Just n -> anyCollision n

        (collisionBubbles, nonCollisionBubbles)=List.partition collisionTest bShapes

        clearRenderable=clear (0,0) width height
        gridRenderable = gridShapes tree
        colBub = bubbleShapes collisionBubbles True
        nonColBub = bubbleShapes nonCollisionBubbles False  
    in
        div
            []
            [ div [][]
            , button [onClick ClearAll][Html.text "Reset"]
            , Canvas.toHtml (width, height)
                [on "mousedown" (Json.Decode.map MouseDown decoder)]
                (
                    [clearRenderable, gridRenderable, colBub, nonColBub]
                )
            , div[][Html.text ("Bubbles:"++(String.fromInt (List.length model.bubbles)))]
            , div[][Html.text ("Click mouse to add some more bubbles")]
            ]

subscriptions model =Time.every 25 (\t->Tick)

main = Browser.element 
    { init=init
    , update=update
    , view=view
    , subscriptions=subscriptions
    }
