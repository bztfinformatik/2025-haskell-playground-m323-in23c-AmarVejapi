import GHC.Natural (Natural)

capital :: Fractional a => a -> a -> Natural -> a
capital d i t
  | t == 0 = 0
  | otherwise = (capital d i (t - 1) + d) * (1 + i / 100)
-- capital 1000 5 0 -> 0
-- capital 1000 5 1 -> 1050.0
-- capital 1000 5 2 -> 2152.5
-- capital 1000 5 3 -> 3305.625


druglevelTimeSeries :: Fractional a => a -> a -> Natural -> [(Natural, a)]
druglevelTimeSeries m r t
  | t == 0 = [(0, 0)]
  | otherwise = (t, level) : druglevelTimeSeries m r (t - 1)
  where
    level = m + remaining * (1 - r / 100)
    remaining = snd (head (druglevelTimeSeries m r (t - 1)))
-- druglevelTimeSeries 100 40 12


egcd :: Integral a => a -> a -> a
egcd a b
  | b == 0 = a
  | otherwise = egcd b (a `mod` b)
-- egcd 48 18 -> 6
