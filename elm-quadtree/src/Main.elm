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

-- Canvas size
width=700
height=700

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
    { bubbles: List Bubble,
      toAdd: Int
    }

-- Initial model: empty bubble list and no command
init: ()-> (Model, Cmd Msg)
init _= ( {bubbles=[], toAdd=0}
        , Cmd.none
        )

randomPosVel: Random.Generator ( ( Float, Float ), ( Float, Float ) )
randomPosVel = Random.pair (Random.pair (Random.float 0 width) (Random.float 0 height)) (Random.pair (Random.float -30 30) (Random.float -30 30))

type Msg = 
    ClearAll
    | NewBubble ((Float,Float),(Float,Float))
    | Tick
    | MouseDown MouseDownData

newRandomBubbleCommand = Random.generate NewBubble randomPosVel

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
        let
            newBubbles =  model.bubbles |> List.map updateBubble
            cmd=if (model.toAdd>0)
                then newRandomBubbleCommand
                else Cmd.none
        in 
            ({model | bubbles=newBubbles, toAdd=Basics.max 0 model.toAdd-1}, cmd)

    NewBubble (pos,vel) ->
        ({model | bubbles=({pos=pos,vel=vel,radius=5,crash=False})::model.bubbles}, Cmd.none)
    
    MouseDown data -> ({model| toAdd=model.toAdd+26}, Cmd.none)

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
                            , stroke (Color.rgba 0 0 0 1)
                            , lineWidth 1
                    ]
                    [path (0,0) (treeGrids tree)]

bubbleCollision b1 b2 = 
    let
        maxDistSq=(b1.radius+b2.radius)*(b1.radius+b2.radius)
        distSq=QuadTree.Vec2d.distSq b1.pos b2.pos
    in
        distSq<=maxDistSq

toBubbleShape tree bubble = 
    case tree of
        Nothing -> []
        Just n -> 
            let
                anycollisions=QuadTree.QuadTree.collisions n (toShape bubble) 
                                |> List.filter (\s -> s.data/=bubble)
                                |> List.any (\s -> bubbleCollision bubble s.data)
                color=if (anycollisions) 
                        then (Color.rgba 255 0 0 1)
                        else (Color.rgba 0 0 0 1)
            in
                [shapes
                    [transform [translate (0) (0)]
                            , stroke (Color.rgba 0 0 0 1)
                            , lineWidth 1
                            , fill color
                    ]
                    [circle bubble.pos bubble.radius]
                ]
                

-- Creates the view for a given max-value
view model= 
    let
        bubbleShapes=(model.bubbles |> List.map toShape)
        tree=QuadTree.QuadTree.create 3 bubbleShapes
    in
        div
            []
            [ div [][]
            , button [onClick ClearAll][Html.text "Reset"]
            , Canvas.toHtml (width, height)
                [on "mousedown" (Json.Decode.map MouseDown decoder)]
                (
                    [ clear (0,0) width height]
                    ++
                    [(gridShapes tree)]
                    ++
                    List.concat (model.bubbles |> List.map (toBubbleShape tree)) 
                )
            , div[][Html.text ("Bubbles:"++(String.fromInt (List.length model.bubbles)))]
            , div[][Html.text ("Click mouse to add bubbles")]
            ]

subscriptions model =Time.every 5 (\t->Tick)

main = Browser.element 
    { init=init
    , update=update
    , view=view
    , subscriptions=subscriptions
    }
