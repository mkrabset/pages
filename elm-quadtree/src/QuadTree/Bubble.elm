module QuadTree.Bubble exposing (..)

import QuadTree.Vector2d exposing (Vector2d,add,multiply)
import QuadTree.Vector2d exposing (subtract,sqLength,dot)
import Html exposing (s)


-- Bubble type
type alias Bubble=
    { pos: Vector2d
    , vel: Vector2d
    , radius: Float
    }

-- Takes a value for remainingTickTime and two bubbles and returns the time of the next collision, if it's below remainingTickTime
-- , or Nothing if no collision will happend
nextCollision: Float -> Bubble -> Bubble -> Maybe Float
nextCollision deadline b1 b2 =
    if (dot (subtract b1.vel b2.vel) (subtract b1.pos b2.pos) >=0) then Nothing
    else
        let
            relPos = subtract b1.pos b2.pos
            relVel = subtract b1.vel b2.vel
            dist=b1.radius+b2.radius
        
            q=sqLength relVel
            r=2*(relPos.x*relVel.x + relPos.y*relVel.y)
            s=(sqLength relPos) - (dist*dist)

            det=r*r-4*q*s 
        in
            if (det>0) then
                let
                    t1=(-r + (sqrt det)) / (2*q)
                    t2=(-r - (sqrt det)) / (2*q)
                    t1Smallest = t1 < t2
                in
                    if (t1Smallest && t1>0 && t1<deadline) then Just t1
                    else if (not t1Smallest && t2>0 && t2<deadline) then Just t2
                    else Nothing
            else Nothing