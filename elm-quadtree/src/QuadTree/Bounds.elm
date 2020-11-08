module QuadTree.Bounds exposing (..)

import QuadTree.Vector2d exposing (Vector2d)

type alias Bounds = 
    { center: Vector2d
    , rx: Float
    , ry: Float
    }

mergePair: Bounds -> Bounds -> Bounds
mergePair b1 b2 = 
    let 
        minx = min (b1.center.x - b1.rx) (b2.center.x - b2.rx)
        maxx = max (b1.center.x + b1.rx) (b2.center.x + b2.rx)
        miny = min (b1.center.y - b1.ry) (b2.center.y - b2.ry)
        maxy = max (b1.center.y + b1.ry) (b2.center.y + b2.ry)
        center = { x = (minx + maxx) / 2, y = (miny + maxy) / 2}
    in
        { center = center
        , rx = (maxx - minx) / 2
        , ry = (maxy - miny) / 2
        }

merge: List Bounds -> Maybe Bounds
merge lb = 
    case lb of
        [] -> Nothing
        h::t -> Just (List.foldl mergePair h t)

overlap: Bounds -> Bounds -> Bool
overlap b1 b2 =
    if (b1.center.x + b1.rx < b2.center.x - b2.rx) then False          -- b1 is left of b2
    else if (b1.center.x - b1.rx > b2.center.x + b2.rx) then False     -- b1 is right of b2
    else if (b1.center.y + b1.ry < b2.center.y - b2.ry) then False     -- b1 is below b2
    else if (b1.center.y - b1.ry > b2.center.y + b2.ry) then False     -- b1 is above b2
    else True