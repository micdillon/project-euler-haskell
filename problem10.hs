--
-- problem10.hs
--

sieve :: (Floating a, RealFrac a, Integral b) => a -> b -> [b] -> [b]
sieve c i ns
  | i >= m = ns
  | i <  m = sieve c n fs
  where 
    m = floor $ sqrt c
    fs = filter (\n-> n<=i || n`mod`i/=0) ns
    n = nxt (i+1) fs
    nxt n ns
      | n `elem` ns = n
      | n >= m      = m
      | otherwise   = nxt (n+1) ns

ans :: Integer
ans = sum $ sieve 2000000 2 [2..2000000]
