module Main exposing (main)

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
width=2000
height=1200

-- Branching angle
angle=pi/24

-- Segment length
segLength=10

-- Oddity test
odd: Int->Bool
odd n = (modBy 2 n == 1)

-- Collatz sequence climbing down from given number
collatz: Int -> List Int
collatz n = if (n==1) 
    then [1]
    else if (odd n) then n::collatz((n*3+1)//2)
    else n::collatz (n//2)

-- List of reversed collatz sequences up to given start-number
collatzList: Int -> List (List Int)
collatzList n = List.range 1 n |> List.map collatz  |> List.map List.reverse

-- Matrix for left turn
leftTurn: Matrix
leftTurn=rotation (-angle)

-- Matrix for right turn
rightTurn: Matrix
rightTurn=rotation angle

-- Matrix for segment translation
move: Matrix
move = translation (segLength,0)

-- Node type for collatz tree
type CollatzNode = None | Node Int Vector CollatzNode CollatzNode

-- Turn-and-move operations
turnLeftAndMove m = multiply m (multiply move leftTurn)
turnRightAndMove m = multiply m (multiply move rightTurn)

-- Combine method for adding a collatz (sub)sequence to a collatz-(sub)tree
-- Arguments: 
--   node = node to apply sequence to
--   list = remaining part of sequence
--   m = current matrix transform
combine: CollatzNode -> List Int -> Matrix -> CollatzNode
combine node list m = 
    case list of 
        [] -> node
        h::t ->
            if (odd h) then
                case node of 
                    None -> 
                        Node 1 (apply m (0,0)) (combine None t (turnLeftAndMove m)) None
                    Node w p left right ->
                        Node (w+1) p (combine left t (turnLeftAndMove m)) right
            else
                case node of 
                    None -> 
                        Node 1 (apply m (0,0)) None (combine None t (turnRightAndMove m))
                    Node w p left right ->
                        Node (w+1) p left (combine right t (turnRightAndMove m))

-- Turns a list of reversed collatz sequences into a collatz-tree (needs an empty start-node since it's recursive) 
combineLists: List (List Int) -> CollatzNode -> CollatzNode
combineLists lists rootNode =
    case lists of 
        [] -> rootNode
        h::t -> combineLists t (combine rootNode h Graphics2D.Matrix.identity)

-- Creates a collatz-tree of 'size' n
collatzTree n = combineLists (collatzList n) None

-- Weighted line segment
type alias WeightedLine = { from : Vector, to : Vector, w : Int }

-- Turns a collatz-tree into a list of weighted line segments
collectLines: CollatzNode -> List WeightedLine
collectLines czNode = 
    case czNode of
        None -> []
        Node _ sp left right ->
            let
                lb=case left of
                    None -> []
                    Node w ep _ _ -> [{from=sp, to=ep, w=w}] 
                rb=case right of
                    None -> []
                    Node w ep _ _ -> [{from=sp, to=ep, w=w}] 
            in 
                lb++rb++(collectLines left)++(collectLines right)

-- Returns list of linesegments for a collatz-tree of given size (sorted by weight ascending)
sortedTree: Int -> List WeightedLine
sortedTree n = collectLines(collatzTree n) |> List.sortBy (\l -> l.w)

-- Adds a list of weighted line segments to a dictionary, grouping all lines with same weights together
dictInsert: Dict Int (List WeightedLine) -> List WeightedLine -> Dict Int (List WeightedLine)
dictInsert d lines = case lines of
    [] -> d
    h::t -> case Dict.get h.w d of 
        Nothing -> dictInsert (Dict.insert h.w [h] d) t
        Just l -> dictInsert (Dict.insert h.w (h::l) d) t

-- Creates a dictionary with weight as key and list of line-segments as value (for collatz-tree of given size)
linesByWeight: Int -> Dict Int (List WeightedLine)
linesByWeight n = dictInsert Dict.empty (sortedTree n)

-- Maps a list of WeightedLines to a list of PathSegments (usable by Canvas lib)
weightedLinesToPathSeg: List WeightedLine -> List PathSegment
weightedLinesToPathSeg wLines = wLines |> List.map (\l->[moveTo l.from, lineTo l.to]) |> List.concat

-- Given a weight, maxWeight and a list of WeightedLines, turns it into a renderable with logarithmic alphas
toLineShape: Int -> Int -> List WeightedLine -> Renderable
toLineShape w max l = shapes [ transform [translate 1100 1200, rotate (-pi/2)], stroke (Color.rgba 0 0 0 (toAlpha w max)), lineWidth (logBase 10 (toFloat w))] [ path (0,0) (weightedLinesToPathSeg l)]

-- Turns a collatz-dictionary and a max-weight into a list of Renderables
toLineShapes: Dict Int (List WeightedLine) -> Int -> List Renderable
toLineShapes d max = Dict.keys d |> List.map (\k -> Dict.get k d |>Maybe.withDefault [] |> toLineShape k max)

-- Calculates logarithmic alpha based on weight and maxWeight
toAlpha: Int -> Int -> Float
toAlpha w m = (logBase e (toFloat w))/(logBase e (toFloat m))

-- Creates the view for a given max-value
view max = Canvas.toHtml (width, height)
            [ style "border" "none" ]
            ([]++(toLineShapes (linesByWeight max) max))

main = view 50000



