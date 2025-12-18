
-- Arithmetisches Mittel
arithmeticMean :: Fractional a => a -> a -> a
arithmeticMean a b = (a + b) / 2

-- Geometrisches Mittel
geometricMean :: Floating a => a -> a -> a
geometricMean a b = sqrt (a * b)

-- Harmonisches Mittel
harmonicMean :: Fractional a => a -> a -> a
harmonicMean a b = 2 * a * b / (a + b)





interpolate :: Fractional a => (a -> a -> a) -> [a] -> [a]
interpolate f [] = []
interpolate f [x] = [x]
interpolate f (x:y:xs) = x : f x y : interpolate f (y:xs)

interpolateN :: Fractional a => Int -> (a -> a -> a) -> [a] -> [a]
interpolateN 0 f xs = xs
interpolateN n f xs = interpolateN (n-1) f (interpolate f xs)
