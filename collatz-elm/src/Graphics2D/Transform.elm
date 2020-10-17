module Graphics2D.Transform exposing (..)

import Graphics2D.Matrix exposing (..)
import Graphics2D.Vector exposing (..)

-- Returns matrix for rotation at a given angle
rotation: Float -> Matrix  
rotation angle = ((cos(angle), -(sin(angle)),0),(sin(angle), cos(angle),0),(0,0,1))

-- Returns matrix for the given translation
translation: Vector -> Matrix
translation v = case v of
    (x,y) -> ((1,0,x),(0,1,y),(0,0,1))

-- Applies the given transform to the given vector
apply: Matrix -> Vector -> Vector
apply m v = case (m,v) of
    (((m11,m21,m31),(m12,m22,m32),(m13,m23,m33)), (vx,vy)) -> 
        (m11*vx+m21*vy+m31, m12*vx+m22*vy+m32)
