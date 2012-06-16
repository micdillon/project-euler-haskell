--
-- problem4.hs
--

dgts :: Integer -> [Integer]
dgts n
  | a == 0    = [(n `mod` 10)]
  | otherwise = (n `mod` 10) : dgts a
  where a = n `div` 10

agr :: [Integer] -> Integer
agr []     = 0
agr (n:ns) = ((10 ^ p) * n) + (agr ns)
  where p = length ns

plndrm :: Integer -> Bool
plndrm n = n == agr (dgts n)

-- > let ps = [x*y| x<-[100..999], y<-[100..999], plndrm (x*y)]
-- > maximum ps
-- this returned 906609 which is correct, but i want to figure
-- out how to count down to the solution, thus making a faster algorithm.

-- this algorithm won't work, unfortunately...
cntdwn :: Integer -> Integer -> [(Integer,Integer,Integer,Bool)]
cntdwn i j
  | i == 100     = [(i, j, i*j, plndrm (i*j))]
  | i > j        = (i, j, i*j, plndrm (i*j)) : cntdwn (i-1) j
  | i == j       = (i, j, i*j, plndrm (i*j)) : cntdwn 999 (j-1)
