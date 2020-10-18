module Graphics2D.Matrix exposing (..)

-- Type alias for 3x3 matrix
type alias Matrix = ((Float,Float,Float),(Float,Float,Float),(Float,Float,Float))

-- 3x3 identity matrix
identity: Matrix
identity = ((1,0,0),(0,1,0),(0,0,1))

-- Multiplies the given two matrices
multiply: Matrix -> Matrix -> Matrix
multiply a b = case (a,b) of
    (  ((a11,a21,a31),(a12,a22,a32),(a13,a23,a33)), ((b11,b21,b31),(b12,b22,b32),(b13,b23,b33)) ) ->
        let 
          c1 = (a11*b11+a21*b12+a31*b13, a11*b21+a21*b22+a31*b23,a11*b31+a21*b32+a31*b33)
          c2 = (a12*b11+a22*b12+a32*b13, a12*b21+a22*b22+a32*b23,a12*b31+a22*b32+a32*b33)
          c3 = (a13*b11+a23*b12+a33*b13, a13*b21+a23*b22+a33*b23,a13*b31+a23*b32+a33*b33)
        in
          (c1,c2,c3)

