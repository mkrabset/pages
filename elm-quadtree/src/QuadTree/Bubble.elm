module QuadTree.Bubble exposing (..)

import QuadTree.Vector2d exposing (Vector2d,add,multiply,subtract,sqLength,dot,norm,neg)
import Html exposing (s)


-- Bubble type
type alias Bubble=
    { pos: Vector2d
    , vel: Vector2d
    , radius: Float
    , collisions: Int
    }

-- Takes a value for remainingTickTime and two bubbles and returns the time of the next collision, if it's below remainingTickTime
-- , or Nothing if no collision will happend
nextCollision: Float -> Bubble -> Bubble -> Maybe Float
nextCollision deadline b1 b2 =
    if (dot (subtract b1.vel b2.vel) (subtract b1.pos b2.pos) >=0) then Nothing
    else
        let
            margin=0.000000001
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
                    t1=100 --(-r + (sqrt det)) / (2*q)
                    t2=(-r - (sqrt det)) / (2*q)
                    t1Smallest = t1 < t2
                in
                    if (t1Smallest && t1>margin && t1<deadline) then Just t1
                    else if (not t1Smallest && t2>margin && t2<deadline) then Just t2
                    else Nothing
            else Nothing


bubbleCollision: Bubble -> Bubble -> Bool
bubbleCollision b1 b2 = 
    let
        maxDistSq=(b1.radius + b2.radius)*(b1.radius + b2.radius)
        distSq=sqLength (subtract b1.pos b2.pos)
    in
        distSq<=maxDistSq


        -- Apply collision to bubbles
collide: Bubble -> Bubble -> (Bubble,Bubble)
collide b1 b2 = 
    let
        relVel=subtract b1.vel b2.vel
        relPos=subtract b1.pos b2.pos
        normRelVel=norm relVel
        normRelPos=norm relPos
        velChange=multiply (dot normRelVel normRelPos) relVel
    in
    ({b1| vel = add b1.vel (neg velChange),collisions=b1.collisions+1},{b2 | vel = add b2.vel velChange, collisions=b2.collisions+1})



