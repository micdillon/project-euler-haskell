--
-- problem12.hs
--
import Control.Applicative

dvsrs :: (Eq a, Num a, Integral a) => a -> [a]
dvsrs n = hlpr n n []
  where 
    hlpr n i ds
      | i == 1    = 1 : ds
      | a == True = hlpr n (i-1) (i:ds)
      | otherwise = hlpr n (i-1) ds
        where a = n `mod` i == 0

-- exploiting the fact that the nth triangle number
-- equals (n*(n+1))/2. 
tdvs :: (Eq a, Num a, Integral a) => a -> [a]
tdvs n
  | even n    = (*) <$> dvsrs (n`div`2) <*> dvsrs (n+1)
  | otherwise = (*) <$> dvsrs n <*> dvsrs ((n+1)`div`2)

-- still need some optimization because ans
-- takes about 4 minutes to run
ans = hlpr 1
  where
    hlpr n
      | length t > 500 = (n*(n+1))`div`2
      | otherwise      = hlpr (n+1)
        where 
          t = tdvs n
