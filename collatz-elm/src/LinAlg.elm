-- Module for some linear algebra
module LinAlg exposing (..)

-- Type alias for 3x3 matrix
type alias Matrix = ((Float,Float,Float),(Float,Float,Float),(Float,Float,Float))

-- Type alias for 2d vector
type alias Vector = (Float,Float)

-- 3x3 identity matrix
identityMatrix: Matrix
identityMatrix = ((1,0,0),(0,1,0),(0,0,1))

-- Multiplies the given two matrices
matrixMult: Matrix -> Matrix -> Matrix
matrixMult a b = case (a,b) of
    (  ((a11,a21,a31),(a12,a22,a32),(a13,a23,a33)), ((b11,b21,b31),(b12,b22,b32),(b13,b23,b33)) ) ->
        let 
          c1 = (a11*b11+a21*b12+a31*b13, a11*b21+a21*b22+a31*b23,a11*b31+a21*b32+a31*b33)
          c2 = (a12*b11+a22*b12+a32*b13, a12*b21+a22*b22+a32*b23,a12*b31+a22*b32+a32*b33)
          c3 = (a13*b11+a23*b12+a33*b13, a13*b21+a23*b22+a33*b23,a13*b31+a23*b32+a33*b33)
        in
          (c1,c2,c3)
      
-- Returns matrix for rotation at a given angle
rotation: Float -> Matrix  
rotation angle = ((cos(angle), -(sin(angle)),0),(sin(angle), cos(angle),0),(0,0,1))

-- Returns matrix for the given translation
translation: Vector -> Matrix
translation v = case v of
    (x,y) -> ((1,0,x),(0,1,y),(0,0,1))

-- Applies the given transform to the given vector
transformVector: Matrix -> Vector -> Vector
transformVector m v = case (m,v) of
    (((m11,m21,m31),(m12,m22,m32),(m13,m23,m33)), (vx,vy)) -> 
        (m11*vx+m21*vy+m31, m12*vx+m22*vy+m32)

vectorAdd: Vector -> Vector -> Vector
vectorAdd (x1,y1) (x2,y2) = (x1+x2,y1+y2)

vectorScalarMult: Float -> Vector -> Vector
vectorScalarMult f (x,y)=(f*x, f*y)