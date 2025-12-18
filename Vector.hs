{-# LANGUAGE GADTs #-}

module Vector
( Vector (Vector),
    vadd,
    vsub,
    vlength,
    vunit,
    vmult,
    vrotate
)
where

-- keyword data: definition of new type somtimes called: TYPE CONSTRUCTOR (not to be confused with the VALUE CONSTRUCTOR within type definition)
data Vector where

-- VALUE CONSTRUCTOR (sometimes called DATA CONSTRUCTOR): Vector
-- arguments of constructor: two Doubles, namely x- and y-value
-- result of constructor: Vector
    Vector :: Double -> Double -> Vector

-- Vector shall be an instance of type class Show
-- type class Show provides a function which can print out a Vector to a String
    deriving (Show)

-- Vektor-Funktionen:

vadd :: Vector -> Vector -> Vector
vadd (Vector a1 a2) (Vector b1 b2) = Vector (a1 + b1) (a2 + b2)

vsub :: Vector -> Vector -> Vector
vsub (Vector a1 a2) (Vector b1 b2) = Vector (a1 - b1) (a2 - b2)

vlength :: Vector -> Double
vlength (Vector a1 a2) = sqrt (a1 * a1 + a2 * a2)

vunit :: Vector -> Vector
vunit v@(Vector a1 a2) =
    let len = vlength v in Vector (a1 / len) (a2 / len)

vmult :: Double -> Vector -> Vector
vmult k (Vector a1 a2) = Vector (k * a1) (k * a2)

vrotate :: Double -> Vector -> Vector
vrotate alpha (Vector a1 a2) =
    Vector (a1 * cos alpha - a2 * sin alpha) (a1 * sin alpha + a2 * cos alpha)
