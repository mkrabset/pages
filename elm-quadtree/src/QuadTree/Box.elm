module QuadTree.Box exposing (..)

import QuadTree.Vec2d exposing (..)

type Quadrant = NW | NE | SW | SE

type alias Box = (Vec2d,Vec2d)

create: Vec2d -> Vec2d -> Box
create (x1,y1) (x2,y2)=
    ((min x1 x2, min y1 y2),(max x1 x2, max y1 y2))

merge: Box -> Maybe Box -> Maybe Box
merge b1 b2 =
    case b2 of 
        Nothing -> Just b1
        Just ((bx1,by1),(bx2,by2)) -> 
            let 
                ((ax1,ay1),(ax2,ay2))=b1 
            in
                Just ( ((min (min ax1 ax2) (min bx1 bx2)), (min (min ay1 ay2) (min by1 by2))) 
                     , ((max (max ax1 ax2) (max bx1 bx2)), (max (max ay1 ay2) (max by1 by2)))
                     )

bounds: List Box -> Maybe Box
bounds boxes = case boxes of 
                [] -> Nothing
                h::t -> List.foldr merge (Just h) t

quadrant: Box -> Box -> Maybe Quadrant
quadrant outerbox b = 
    let
        ((bx1,by1),(bx2,by2))=outerbox
        mx=(bx1+bx2)/2
        my=(by1+by2)/2
        ((x1,y1),(x2,y2))=b
    in
        if (x2<mx) then --left
            if (y1>my) then --top
                Just NW
            else if (y2<my) then --bottom
                Just SW
            else
                Nothing
        else if (x1>mx) then --right
            if (y1>my) then --top
                Just NE
            else if (y2<my) then --bottom
                Just SE
            else
                Nothing
        else 
            Nothing

overlap: Box -> Box -> Bool
overlap a b = 
    let
        ((ax1,ay1),(ax2,ay2))=a
        ((bx1,by1),(bx2,by2))=b
    in
        if ((ax2<bx1) || (ax1 > bx2)) then False -- a left of b, or a right of b 
        else if ((ay2<by1) || (ay1 > by2)) then False -- a below b, or a above b
        else True

