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
import QuadTree.Vec2d exposing (..)
import Json.Decode exposing (Decoder, int, at, map)
import QuadTree.Box exposing (Box)
import QuadTree.QuadTree
import QuadTree.QuadTree
import QuadTree.QuadTree

timeDeltaMillis=10
maxSpeed = 100

-- Canvas size
width=1200
height=700

bubblesAddedForEachClick=50

type alias MouseDownData = 
    { offsetX : Int
    , offsetY : Int
    }

decoder : Decoder MouseDownData
decoder =
    Json.Decode.map2 MouseDownData
        (at [ "offsetX" ] int)
        (at [ "offsetY" ] int)

type alias Bubble=
    { pos: Vec2d
    , vel: Vec2d
    , radius: Float
    , crash: Bool
    }

type alias Model= 
    { bubbles: List Bubble
    }

-- Initial model: empty bubble list and no command
init: ()-> (Model, Cmd Msg)
init _= ( {bubbles=[]}
        , Cmd.none
        )

randomPosVel: Random.Generator ( ( Float, Float ), ( Float, Float ) )
randomPosVel = Random.pair (Random.pair (Random.float 0 width) (Random.float 0 height)) (Random.pair (Random.float -maxSpeed maxSpeed) (Random.float -maxSpeed maxSpeed))

type Msg = 
    ClearAll
    | NewBubble Int ((Float,Float),(Float,Float))
    | Tick
    | MouseDown MouseDownData

newRandomBubbleCommand num = Random.generate (NewBubble num) randomPosVel

modval mod v = if (v>mod) then v-mod
                else if (v<0) then v+mod
                else v

updateBubble: Bubble -> Bubble
updateBubble bubble = 
    let 
        (x,y)=QuadTree.Vec2d.add bubble.pos (QuadTree.Vec2d.mul (timeDeltaMillis/1000.0) bubble.vel)
        adjPos=(modval width x, modval height y)
    in
        {bubble | pos=adjPos}

update: Msg -> Model -> (Model,Cmd Msg)
update msg model = case msg of 
    ClearAll -> init () 

    Tick -> 
        ({model | bubbles=(model.bubbles |> List.map updateBubble)}, Cmd.none)

    NewBubble num (pos,vel) ->
        if (num>0) then
            ({model | bubbles=({pos=pos,vel=vel,radius=5,crash=False})::model.bubbles}, newRandomBubbleCommand (num-1))
        else 
            (model, Cmd.none)
    
    MouseDown data -> (model, newRandomBubbleCommand bubblesAddedForEachClick)

bubbleBounds: Bubble -> Box
bubbleBounds b = ((QuadTree.Vec2d.add b.pos (-b.radius,-b.radius)),(QuadTree.Vec2d.add b.pos (b.radius,b.radius)))

toShape: Bubble -> QuadTree.QuadTree.Shape Bubble
toShape bubble = {bounds=(bubbleBounds bubble), data=bubble}

treeGrids: Maybe (QuadTree.QuadTree.Node Bubble) -> List PathSegment
treeGrids node = case node of 
    Just (QuadTree.QuadTree.Leaf l) -> []
    Just (QuadTree.QuadTree.NonLeaf l) ->
        let
            ((x1,y1),(x2,y2))=l.bounds
            mx=(x1+x2)/2
            my=(y1+y2)/2
            childGrids=[Just l.nw, Just l.ne, Just l.sw, Just l.se] |> List.map treeGrids
        in
            [moveTo (mx,y2), lineTo (mx,y1), moveTo (x1,my), lineTo(x2,my)]++(List.concat childGrids)
    Nothing -> []


gridShapes tree = shapes
                    [transform [translate (0) (0)]
                            , stroke (Color.rgba 0.8 0.8 0.8 1)
                            , lineWidth 1
                    ]
                    [path (0,0) (treeGrids tree)]

bubbleCollision b1 b2 = 
    let
        maxDistSq=(b1.radius+b2.radius)*(b1.radius+b2.radius)
        distSq=QuadTree.Vec2d.distSq b1.pos b2.pos
    in
        distSq<=maxDistSq

anyCollision: QuadTree.QuadTree.Node Bubble -> QuadTree.QuadTree.Shape Bubble -> Bool                
anyCollision tree bubbleShape = QuadTree.QuadTree.collisions tree bubbleShape 
                                |> List.filter (\s -> s.data/=bubbleShape.data)
                                |> List.any (\s -> bubbleCollision bubbleShape.data s.data)

-- Creates the view for a given max-value
view model= 
    let
        bubbleShapes=(model.bubbles |> List.map toShape)
        tree=QuadTree.QuadTree.create 3 bubbleShapes

        collisionTest=case tree of 
            Nothing-> (\_->False)
            Just n -> anyCollision n

        (collisionBubbles, nonCollisionBubbles)=List.partition collisionTest bubbleShapes

        clearRenderable=clear (0,0) width height
        gridRenderable = gridShapes tree
        colBub = shapes
                    [transform [translate (0) (0)]
                            , stroke (Color.rgba 0 0 0 1)
                            , lineWidth 1
                            , fill (Color.rgba 1 0 0 1)
                    ]
                    (collisionBubbles |> List.map (\bs -> circle bs.data.pos bs.data.radius))
        nonColBub = shapes
                    [transform [translate (0) (0)]
                            , stroke (Color.rgba 0 0 0 1)
                            , lineWidth 1
                            , fill (Color.rgba 0 1 0 1)
                    ]
                    (nonCollisionBubbles |> List.map (\bs -> circle bs.data.pos bs.data.radius))          
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
            , div[][Html.text ("Click mouse to add bubbles")]
            ]

subscriptions model =Time.every 25 (\t->Tick)

main = Browser.element 
    { init=init
    , update=update
    , view=view
    , subscriptions=subscriptions
    }
