module QuadTree.Bubble exposing (..)

import QuadTree.Vector2d exposing (Vector2d)

-- Bubble type
type alias Bubble=
    { pos: Vector2d
    , vel: Vector2d
    , radius: Float
    }