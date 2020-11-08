module QuadTree.QuadTree exposing (..)
import QuadTree.Bounds exposing (..)
import List exposing (..)

-- Quadrant type
type Quadrant = NW | NE | SW | SE

whichQuadrant: Bounds -> Bounds -> Maybe Quadrant
whichQuadrant outer b = 
    if (b.center.x + b.rx < outer.center.x) then                   --left
        if (b.center.y + b.ry < outer.center.y) then Just SW       --bottom
        else if (b.center.y - b.ry > outer.center.y) then Just NW  --top
        else Nothing
    else if (b.center.x - b.rx > outer.center.x) then              --right
        if (b.center.y + b.ry < outer.center.y) then Just SE       --bottom
        else if (b.center.y - b.ry > outer.center.y) then Just NE  --top
        else Nothing
    else Nothing

quadrant: Bounds -> Quadrant -> Bounds
quadrant b q = 
    let
        dx=b.rx/2
        dy=b.ry/2
        xwest=b.center.x - dx
        xeast=b.center.x + dx
        ysouth=b.center.y - dy
        ynorth=b.center.y + dy
    in
        case q of
            NW -> { center = { x = xwest, y = ynorth}, rx = dx, ry = dy}
            NE -> { center = { x = xeast, y = ynorth}, rx = dx, ry = dy}
            SW -> { center = { x = xwest, y = ysouth}, rx = dx, ry = dy}
            SE -> { center = { x = xeast, y = ysouth}, rx = dx, ry = dy}

-- Generic type for shape with given rectangular bounds
type alias Shape d = 
    { bounds: Bounds
    , data: d
    }

-- QuadTree node, either Leaf with items or NonLeaf with items and quadrant-children
type Node d = 
    Leaf 
        { capacity: Int
        , bounds: Bounds
        , items: List (Shape d)
        } 
    | NonLeaf
        { bounds: Bounds
        , nw: Node d
        , ne: Node d
        , sw: Node d
        , se: Node d
        , items: List (Shape d)
        }


-- Adds a shape to a QuadTree
add: (Shape d) -> (Node d) -> (Node d)
add shape node = case node of
    Leaf l -> 
        if (l.capacity > (List.length l.items))
        then Leaf {l | items=(shape::l.items)}
        else 
            let
                quad = quadrant l.bounds
                nl= NonLeaf 
                    { bounds=l.bounds
                    , nw = Leaf {l | bounds=(quad NW),items=[]}
                    , ne = Leaf {l | bounds=(quad NE),items=[]}
                    , sw = Leaf {l | bounds=(quad SW),items=[]}
                    , se = Leaf {l | bounds=(quad SE),items=[]}
                    , items=[]
                    }
                in
                    List.foldr add nl (shape::l.items)
    NonLeaf nl -> 
        case (whichQuadrant nl.bounds shape.bounds) of
            Just NW -> NonLeaf {nl | nw=(add shape nl.nw)}
            Just NE -> NonLeaf {nl | ne=(add shape nl.ne)}
            Just SW -> NonLeaf {nl | sw=(add shape nl.sw)}
            Just SE -> NonLeaf {nl | se=(add shape nl.se)}
            Nothing -> NonLeaf {nl | items=(shape::nl.items)}


-- Creates a QuadTree from a List of Shapes, or Nothing if the List is empty
create: Int -> (List (Shape d)) -> Maybe (Node d)
create nodeCapacity shapes = 
    let
        bounds = shapes |> List.map (\s -> s.bounds) |> merge
    in
        case bounds of 
            Nothing -> Nothing
            Just b -> Just (List.foldr add (Leaf {capacity=nodeCapacity, bounds=b, items=[]}) shapes)


-- Returns List of all Shapes from a QuadTree that have boundary-collisions with the boundary of the given shape
collisions: Node d -> Shape d -> List (Shape d)
collisions node shape = case node of 
    Leaf l ->
        l.items |> List.filter (\s -> (s.data/=shape.data) && (overlap shape.bounds s.bounds))
    NonLeaf nl -> 
        let
            directCollisions= nl.items 
                |> List.filter (\s -> (s.data/=shape.data) && (overlap shape.bounds s.bounds) )
        in
            case (whichQuadrant nl.bounds shape.bounds) of
                Just NW -> directCollisions++(collisions nl.nw shape)
                Just NE -> directCollisions++(collisions nl.ne shape)
                Just SW -> directCollisions++(collisions nl.sw shape)
                Just SE -> directCollisions++(collisions nl.se shape)
                Nothing -> 
                    let
                        collisionsInQuadrants=List.concat ([nl.nw,nl.ne,nl.sw,nl.se] |> List.map (\qn->collisions qn shape))
                    in
                        directCollisions++collisionsInQuadrants

