--
-- problem5.hs
--

lcd :: Integer -> Integer -> Integer
lcd n i
  | dvsbl n   = n
  | otherwise = lcd (n+i) i
  where dvsbl m = all id $ map (\x-> n `mod` x == 0) [1..i]

-- answer: 232792560
