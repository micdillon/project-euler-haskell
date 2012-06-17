--
-- euler.hs
-- -- a collection of functions that seem to recure in solutions
-- -- to euler project problems.
--
module Euler 
(
  dgts
, dvsrs
) where

import Control.Applicative
import Data.List

dgts :: Integer -> [Integer]
dgts n
  | a == 0    = [(n `mod` 10)]
  | otherwise = dgts a ++ [(n `mod` 10)]
  where a = n `div` 10

-- somewhat efficient function for getting the
-- divisors of a number (need to get rid of the 
-- call to nub at some point).
dvsrs :: (Eq a, Num a, Integral a) => a -> [a]
dvsrs n = nub $ dv n 2
  where
    dv n i
      | n == 1    = [1]
      | i == n    = [1,n]
      | d         = (*) <$> dv (n`div`i) 2 <*> [1,i]
      | otherwise = dv n (i+1)
        where d = n`mod`i==0
