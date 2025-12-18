module VFunctions where 
import Sampldata
import Vector

-- Erstes Argument: Liste von Vektoren
-- Zweites Argument: Richtungsvektor
-- Ergebnis: Liste von Vektoren, die alle um den Richtungsvektor verschoben sind
vshift :: [Vector] -> Vector -> [Vector]
vshift vecs v = map (\vec -> vadd vec v) vecs


-- Erstes Argument: Liste von Vektoren
-- Ergebnis: Liste von Vektoren, die im NW-Sektor liegen

-- Variante 1: Mit List-Comprehension, aber ohne filter
pointsNW :: [Vector] -> [Vector]
pointsNW = filter (\(Vector x y) -> (x <= 0) && (y >= 0))

-- Variante 2: Mit filter, aber ohne List-Comprehension
pointsNW' :: [Vector] -> [Vector]
pointsNW' vecs = filter (\(Vector x y) -> (x <= 0) && (y >= 0)) vecs


-- Erstes Argument: Liste von Vektoren
-- Ergebnis: kleinster y-Wert (Double)

-- Variante 1: Rekursiv, aber ohne foldl/foldr
minY :: [Vector] -> Double
minY [Vector _ y] = y
minY (Vector _ y : vecs) =
    let minRest = minY vecs
    in if y < minRest then y else minRest

-- Variante 2: Mit foldl, aber ohne Rekursion
minY' :: [Vector] -> Double
minY' (Vector _ y : vecs) = foldl (\acc (Vector _ y') -> if y' < acc then y' else acc) y vecs

