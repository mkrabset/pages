module PerlinNoise exposing (..)

-- Trivially translated from C code on wikipedia: https://en.wikipedia.org/wiki/Perlin_noise#Algorithm_detail

interpolate: Float -> Float -> Float -> Float
interpolate a b t = (1.0-t)*a + t*b

randomGradient: Float -> Float -> ( Float, Float )
randomGradient ix iy = 
    let
        random = 2920.0 * (sin (ix * 21942.0 + iy * 171324.0 + 8912.0)) * (cos(ix * 23157.0 * iy * 217832.0 + 9758.0))
    in 
        (cos random, sin random)

dotGridGradient: Float -> Float -> Float -> Float -> Float
dotGridGradient ix iy x y =
    let
        -- Get gradient from integer coordinates
        (gx,gy) = randomGradient ix iy  

        -- Compute the distance vector
        dx = x - ix
        dy = y - iy
    in 
        -- Compute the dot-product
        dx*gx + dy*gy

perlin: (Float,Float) -> Float
perlin (x,y) = 
    let
        -- Grid cell coordinates
        x0 = floor x
        x1 = x0 + 1
        y0 = floor y
        y1 = y0 + 1

    -- Interpolation weights
        sx = x - toFloat x0
        sy = y - toFloat y0

    -- Interpolate between grid point gradients
        ix0 = interpolate (dotGridGradient (toFloat x0) (toFloat y0) x y) (dotGridGradient (toFloat x1) (toFloat y0) x y) sx
        ix1 = interpolate (dotGridGradient (toFloat x0) (toFloat y1) x y) (dotGridGradient (toFloat x1) (toFloat y1) x y) sx
    in
        interpolate ix0 ix1 sy 
