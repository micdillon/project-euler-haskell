--
-- problem12.hs
--
module Main where

import Euler
import Control.Applicative

-- exploiting the fact that the nth triangle number
-- equals (n*(n+1))/2. 
tdvs :: (Eq a, Num a, Integral a) => a -> [a]
tdvs n
  | even n    = (*) <$> dvsrs (n`div`2) <*> dvsrs (n+1)
  | otherwise = (*) <$> dvsrs n <*> dvsrs ((n+1)`div`2)

ans = hlpr 1
  where
    hlpr n
      | length t > 500 = (n*(n+1))`div`2
      | otherwise      = hlpr (n+1)
        where 
          t = tdvs n

main = do
  putStrLn ("answer: " ++ show ans)
