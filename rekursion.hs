import GHC.Natural (Natural)

capital :: Fractional a => a -> a -> Natural -> a
capital d i t
  | t == 0 = 0
  | otherwise = (capital d i (t - 1) + d) * (1 + i / 100)
-- capital 1000 5 0 -> 0
-- capital 1000 5 1 -> 1050.0
-- capital 1000 5 2 -> 2152.5
-- capital 1000 5 3 -> 3305.625

-- Lösung:
-- function capital
-- d: amount of regular deposits
-- i: interest rate (percentage)
-- t: duration
capital' :: Fractional a => a -> a -> Natural -> a
capital' d i t
  | t == 0 = 0
  | otherwise = previousCapital + d + yield
  where
    previousCapital = capital' d i (t - 1)
    yield = (previousCapital + d) * (i / 100)



druglevelTimeSeries :: Fractional a => a -> a -> Natural -> [(Natural, a)]
druglevelTimeSeries m r t =
    reverse [(day, level day) | day <- [0..t]]
  where
    decay = 1 - r / 100
    level n
      | n == 0 = 0
      | otherwise = (level (n-1) + m) * decay
-- druglevelTimeSeries 100 40 12

-- Lösung:
-- create a time series of drug levels
-- m: daily administered dose
-- r: brake down rate in percentage
-- t: days (length of time series)
druglevelTimeSeries' :: Fractional a => a -> a -> Natural -> [(Natural, a)]
-- pattern for day 0, ensure to terminate recursion
druglevelTimeSeries' _ _ 0 = [(0, 0)]
-- pattern for subsequent days, prepend current day to previous days
druglevelTimeSeries' m r t = (t, currentLevel) : previousDays
  where
    previousDays = druglevelTimeSeries' m r (t - 1)
    -- use pattern matching to get second element of first tuple in list
    ((_, previousLevel) : _) = previousDays
    currentLevel = (previousLevel + m) * (1 - r / 100)



egcd :: Integral a => a -> a -> a
egcd a b
  | b == 0 = a
  | otherwise = egcd b (a `mod` b)
-- egcd 48 18 -> 6

-- Lösung:
-- compute gcd (Euclidian algorithm - greatest common divisor)
egcd' :: Integral a => a -> a -> a
egcd' p q
  | remainder == 0 = n
  | otherwise = egcd' n remainder
  where
    m = max (abs p) (abs q)
    n = min (abs p) (abs q)
    remainder = mod m n
