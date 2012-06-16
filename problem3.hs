--
-- problem3.hs
--

-- n = 600851475143

-- get the lowest divisor
ld :: Integer -> Integer -> Integer
ld n i
  | n `mod` i == 0 = i
  | otherwise      = ld n (i+1)

fctr :: Integer -> [Integer]
fctr n
  | d == n    = [n]
  | otherwise = d : fctr (n `div` d)
  where d = ld n 2
