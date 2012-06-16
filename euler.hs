--
-- euler.hs
-- -- a collection of functions that seem to recure in solutions
-- -- to euler project problems.
--

module Euler 
(
  dgts
) where

dgts :: Integer -> [Integer]
dgts n
  | a == 0    = [(n `mod` 10)]
  | otherwise = dgts a ++ [(n `mod` 10)]
  where a = n `div` 10
