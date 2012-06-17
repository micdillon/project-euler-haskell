--
-- problem14.hs
--
module Main where

import Data.List

f n
  | even n    = n`div`2
  | otherwise = 3*n + 1

chain n
  | n == 1    = [1]
  | otherwise = n : chain (f n)

ans = fst a
  where 
    a = maximumBy (\(a,n) (b,m) -> compare n m) [(n, length $ chain n) | n <- [1..999999]]

-- compiling with: ghc -O will produce code that runs under a minute
main = do
  putStrLn ("answer: " ++ show ans)
