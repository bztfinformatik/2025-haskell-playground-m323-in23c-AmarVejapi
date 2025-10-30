import GHC.Natural (Natural)

fib :: Natural -> Natural
fib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)

fibseq :: Natural -> [Natural]
fibseq n = reverse (go n [1, 0])
  where
    go 0 acc = acc
    go k acc@(x:y:_) = go (k - 1) ((x + y) : acc)
