module QuadTree.Vector2d exposing (..)
import Color exposing (yellow)

type alias Vector2d = 
    { x: Float
    , y: Float
    }

fromPair: (Float,Float) -> Vector2d
fromPair (x,y) = 
    { x = x
    , y = y
    }

toPair: Vector2d -> (Float,Float)
toPair v = (v.x, v.y)

zero: Vector2d
zero = 
    { x = 0
    , y = 0
    }

neg: Vector2d -> Vector2d
neg v = 
    { x = -v.x
    , y = -v.y
    }

add: Vector2d -> Vector2d -> Vector2d
add v1 v2 = 
    { x = v1.x + v2.x
    , y = v1.y + v2.y
    }

-- Subtracts the first argument from the second argument vector
subtract: Vector2d -> Vector2d -> Vector2d
subtract v2 v1 = 
    { x = v1.x - v2.x
    , y = v1.x - v2.x
    }

multiply: Float -> Vector2d -> Vector2d
multiply scalar v = 
    { x = v.x * scalar
    , y = v.y * scalar
    }

sum: List Vector2d -> Vector2d
sum vl = 
    List.foldl add zero vl

dot: Vector2d -> Vector2d -> Float
dot v1 v2 = 
    (v1.x * v2.x) + (v1.y * v2.y)

cross: Vector2d -> Vector2d -> Float
cross v1 v2 = 
    (v1.x * v2.y) - (v1.y * v2.x)

sqLength: Vector2d -> Float
sqLength v = dot v v

