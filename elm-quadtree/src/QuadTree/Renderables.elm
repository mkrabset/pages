module QuadTree.Renderables exposing (..)

import Canvas exposing (PathSegment,moveTo,lineTo,Renderable,shapes, path,circle)
import Canvas.Settings exposing (stroke,fill)
import Canvas.Settings.Line exposing (lineWidth)
import Canvas.Settings.Advanced exposing (transform,translate)
import Color

import QuadTree.QuadTree exposing (Node,Shape)
import QuadTree.Bubble exposing (Bubble)
import QuadTree.Vector2d exposing (toPair)

-- Generate grid-lines (as PathSegments) for QuadTree partitioning
treeGrids: Maybe (QuadTree.QuadTree.Node d) -> List PathSegment
treeGrids node = case node of 
    Just (QuadTree.QuadTree.Leaf l) -> []
    Just (QuadTree.QuadTree.NonLeaf l) ->
        let
            (mx, my) = toPair l.bounds.center
            (rx, ry) = (l.bounds.rx, l.bounds.ry)
            childGrids=[Just l.nw, Just l.ne, Just l.sw, Just l.se] |> List.map treeGrids
        in
            [moveTo (mx,my-ry), lineTo (mx,my+ry), moveTo (mx-rx,my), lineTo(mx+rx,my)]++(List.concat childGrids)
    Nothing -> []

-- Generate Renderable for QuadTree partitioning
gridShapes: Maybe (QuadTree.QuadTree.Node d) -> Renderable
gridShapes tree = shapes
                    [transform [translate (0) (0)]
                            , stroke (Color.rgba 0.9 0.9 0.9 1)
                            , lineWidth 1
                    ]
                    [path (0,0) (treeGrids tree)]

bubbleShapes: List (QuadTree.QuadTree.Shape Bubble) -> Bool -> Renderable
bubbleShapes bubbles colliding = 
    let
        color = 
            if (colliding) then Color.rgba 1 0 0 1
            else Color.rgba 0.4 1 0.4 1
    in
        shapes
            [transform [translate (0) (0)]
            , stroke (Color.rgba 0 0 0 1)
            , lineWidth 1
            , fill color
            ]
            (bubbles |> List.map (\bs -> circle (toPair bs.data.pos) bs.data.radius))

border width height =
    shapes
        [transform [translate (0) (0)]
        , stroke (Color.rgba 0.9 0.9 0.9 1)
        , lineWidth 4
        ]
        [path (0,0) [moveTo (0,0), lineTo(0,height), lineTo(width,height), lineTo(width,0), lineTo(0,0)]]