module Polygone where 
import Sampldata
import Vector

data Polygone where
    Square :: Vector -> Vector -> Polygone
    Rectangle :: Vector -> Vector -> Double -> Polygone
    EquilateralTriangle :: Vector -> Vector -> Polygone
    deriving Show

corners :: Polygone -> [Vector]
corners (Square d e) =
    let base = vsub e d
        perp = vrotate (pi / 2) base
        f = vadd e perp
        g = vadd d perp
    in [d, e, f, g]

corners (Rectangle l m h) =
    let base = vsub m l
        perp = vunit (vrotate (pi / 2) base)
        heightVec = vmult h perp
        n = vadd m heightVec
        o = vadd l heightVec
    in [l, m, n, o]

corners (EquilateralTriangle a b) =
    let base = vsub b a
        perp = vunit (vrotate (pi / 2) base)
        h = (vlength base * sqrt 3) / 2
        c = vadd (vadd a (vmult 0.5 base)) (vmult h perp)
    in [a, b, c]



equilateralTriangleCornerThree :: Vector -> Vector -> Vector
equilateralTriangleCornerThree vecA vecB = vecC
    where
        -- first step: compute vector AB
        vecAB = vsub vecB vecA
        -- second step: rotate vector AB by 60 degrees, get vector T
        vecT = vrotate 60 vecAB
        -- third step: compute corner C by adding vector T to point A
        vecC = vadd vecA vecT


-- first argument: vecA - first corner of square
-- second argument: vecB - second corner of square
-- result: vecC - third corner of square
squareCornerThree :: Vector -> Vector -> Vector
squareCornerThree vecA vecB = vecC
    where
        -- first step: compute vector AB
        vecAB = vsub vecB vecA
        -- second step: rotate vector AB by 90 degrees, get vector T
        vecT = vrotate (pi / 2) vecAB
        -- third step: compute corner C by adding vector T to point B
        vecC = vadd vecB vecT


-- first argument: vecA - first corner of square
-- second argument: vecB - second corner of square
-- result: vecD - fourth corner of square
squareCornerFour :: Vector -> Vector -> Vector
squareCornerFour vecA vecB = vecD
    where
        -- first step: compute vector AB
        vecAB = vsub vecB vecA
        -- second step: rotate vector AB by 90 degrees, get vector T
        vecT = vrotate (pi / 2) vecAB
        -- third step: compute corner D by adding vector T to point A
        vecD = vadd vecA vecT


-- first argument: vecA - first corner of rectangle
-- second argument: vecB - second corner of rectangle
-- third argument: height - height of rectangle
-- result: vecC - third corner of rectangle
rectangleCornerThree :: Vector -> Vector -> Double -> Vector
rectangleCornerThree vecA vecB height = vecC
    where
        -- first step: compute vector AB
        vecAB = vsub vecB vecA
        -- second step: compute unit perpendicular vector to AB
        vecPerp = vunit (vrotate (pi / 2) vecAB)
        -- third step: compute height vector
        vecHeight = vmult height vecPerp
        -- fourth step: compute corner C by adding height vector to point B
        vecC = vadd vecB vecHeight


-- first argument: vecA - first corner of rectangle
-- second argument: vecB - second corner of rectangle
-- third argument: height - height of rectangle
-- result: vecC - fourth corner of rectangle
rectangleCornerFour :: Vector -> Vector -> Double -> Vector
rectangleCornerFour vecA vecB height = vecD
    where
        -- first step: compute vector AB
        vecAB = vsub vecB vecA
        -- second step: compute unit perpendicular vector to AB
        vecPerp = vunit (vrotate (pi / 2) vecAB)
        -- third step: compute height vector
        vecHeight = vmult height vecPerp
        -- fourth step: compute corner D by adding height vector to point A
        vecD = vadd vecA vecHeight