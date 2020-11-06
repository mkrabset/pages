module QuadTree.QuadTree exposing (..)
import QuadTree.Box exposing (..)
import List exposing (..)

-- Generic type for shape with given rectangular bounds
type alias Shape d = 
    { bounds: Box
    , data: d
    }

-- QuadTree node, either Leaf with items or NonLeaf with items and quadrant-children
type Node d = 
    Leaf 
        { capacity: Int
        , bounds: Box
        , items: List (Shape d)
        } 
    | NonLeaf
        { bounds: Box
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
        else case l.bounds of 
            ((sx,sy),(ex,ey)) ->
                let
                    mx=(sx+ex)/2
                    my=(sy+ey)/2

                    nl= NonLeaf 
                        { bounds=l.bounds
                        , nw = Leaf {l | bounds=((sx,my),(mx,ey)),items=[]}
                        , ne = Leaf {l | bounds=((mx,my),(ex,ey)),items=[]}
                        , sw = Leaf {l | bounds=((sx,sy),(mx,my)),items=[]}
                        , se = Leaf {l | bounds=((mx,sy),(ex,my)),items=[]}
                        , items=[]
                        }
                in
                    List.foldr add nl (shape::l.items)

    NonLeaf nl -> 
        case quadrant nl.bounds shape.bounds of
            Just NW -> NonLeaf {nl | nw=(add shape nl.nw)}
            Just NE -> NonLeaf {nl | ne=(add shape nl.ne)}
            Just SW -> NonLeaf {nl | sw=(add shape nl.sw)}
            Just SE -> NonLeaf {nl | se=(add shape nl.se)}
            Nothing -> NonLeaf {nl | items=(shape::nl.items)}


-- Creates a QuadTree from a List of Shapes, or Nothing if the List is empty
create: Int -> (List (Shape d)) -> Maybe (Node d)
create nodeCapacity shapes = 
    let
        bounds = shapes |> List.map (\s -> s.bounds) |> QuadTree.Box.bounds
    in
        case bounds of 
            Nothing -> Nothing
            Just b -> Just (List.foldr add (Leaf {capacity=nodeCapacity, bounds=b, items=[]}) shapes)


-- Returns List of all Shapes from a QuadTree that have boundary-collisions with the boundary of the given shape
collisions: Node d -> Shape d -> List (Shape d)
collisions node shape = case node of 
    Leaf l ->
        l.items |> List.filter (\s -> overlap shape.bounds s.bounds)
    NonLeaf nl -> 
        let
            directCollisions= nl.items |> List.filter (\s -> overlap shape.bounds s.bounds)
        in
            case quadrant nl.bounds shape.bounds of
                Just NW -> directCollisions++(collisions nl.nw shape)
                Just NE -> directCollisions++(collisions nl.ne shape)
                Just SW -> directCollisions++(collisions nl.sw shape)
                Just SE -> directCollisions++(collisions nl.se shape)
                Nothing -> 
                    let
                        collisionsInQuadrants=List.concat ([nl.nw,nl.ne,nl.sw,nl.se] |> List.map (\qn->collisions qn shape))
                    in
                        directCollisions++collisionsInQuadrants

