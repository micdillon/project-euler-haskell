--
-- problem6.hs
--

sumOfSqrs n = sum $ map (\x->x*x) [1..n]

sqrOfSums n = s*s
  where s = sum [1..n]

dif n = sqrOfSums n - sumOfSqrs n
