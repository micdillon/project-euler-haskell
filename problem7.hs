--
-- problem7.hs
--

isPrime :: Integer -> [Integer] -> Bool
isPrime n ps = all id $ map (\p->n`mod`p/=0) ps

aggPrimes :: Integer -> Integer -> [Integer] -> [Integer]
aggPrimes i n ps
  | i == 0       = ps
  | isPrime n ps = aggPrimes (i-1) (n+1) (n:ps)
  | otherwise    = aggPrimes i (n+1) ps

-- running $> aggPrimes 10001 2 [2]
-- and taking the second value of the returned list
-- gave the answer: 104743
