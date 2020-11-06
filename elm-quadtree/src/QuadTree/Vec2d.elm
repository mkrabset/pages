module QuadTree.Vec2d exposing (..)

type alias Vec2d = (Float,Float)

add: Vec2d -> Vec2d -> Vec2d
add a b = 
    case (a,b) of 
        ((ax,ay),(bx,by)) -> (ax+bx,ay+by)


neg: Vec2d -> Vec2d
neg v = 
    case v of (x,y) -> (-x,-y)

mul: Float -> Vec2d -> Vec2d
mul scalar v = case v of (x,y) -> (x*scalar, y*scalar)

distSq: Vec2d -> Vec2d -> Float
distSq p1 p2 = 
    let 
        (x1,y1)=p1
        (x2,y2)=p2
    in
        (x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)
