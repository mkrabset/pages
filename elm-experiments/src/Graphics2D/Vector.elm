module Graphics2D.Vector exposing (..)

-- Type alias for 2d vector
type alias Vector = (Float,Float)

origin: Vector
origin = (0,0)

add: Vector -> Vector -> Vector
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

sub: Vector -> Vector -> Vector
sub (x1,y1) (x2,y2) = (x1-x2,y1-y2)

neg: Vector -> Vector
neg (x,y) = (-x, -y)

dot: Vector -> Vector -> Float
dot (x1,y1) (x2,y2) = (x1*x2+y1*y2)

cross: Vector -> Vector -> Float
cross (x1,y1) (x2,y2) = x1*y2 - x2*y1

len: Vector -> Float
len (x,y) = sqrt(x*x + y*y)