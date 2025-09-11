import GHC.Natural (Natural)

fibseq :: Natural -> [Natural]
fibseq n = take (fromIntegral n) (map fib [0..])
  where
    fib 0 = 0
    fib 1 = 1
    fib k = fib (k - 1) + fib (k - 2)

fib :: Natural -> Natural
fib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)


fiblist :: Natural -> [(Natural, Natural)]
fiblist n = zip [0 .. n] (fibseq (n + 1))

c :: Int -> Int -> Int
c n k
  | k == 0    = 1
  | k == n    = 1
  | otherwise = c (n - 1) (k - 1) + c (n - 1) k
