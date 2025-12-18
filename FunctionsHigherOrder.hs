module FunctionsHigherOrder where 
import Sampldata
import Vector
import Data.List

-- Vergleichsfunktion nach Länge
vLengthCompare :: Vector -> Vector -> Ordering
vLengthCompare v1 v2 = compare (vlength v1) (vlength v2)

-- Vergleichsfunktion nach y-Koordinate
vYCompare :: Vector -> Vector -> Ordering
vYCompare (Vector _ y1) (Vector _ y2) = compare y1 y2

-- Vergleichsfunktion Nordost -> Südwest
vNECompare :: Vector -> Vector -> Ordering
vNECompare vecA vecB =
    compare y1 y2
    where
    (Vector _ y1) = vrotate (-135) vecA
    (Vector _ y2) = vrotate (-135) vecB
