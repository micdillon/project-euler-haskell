--
-- problem18.hs
--

import Data.List
import Debug.Trace

tri = [[75]
         ,[95,64]
         ,[17,47,82]
         ,[18,35,87,10]
         ,[20,04,82,47,65]
         ,[19,01,23,75,03,34]
         ,[88,02,77,73,07,63,67]
         ,[99,65,04,28,06,16,70,92]
         ,[41,41,26,56,83,40,80,70,33]
         ,[41,48,72,33,47,32,37,16,94,29]
         ,[53,71,44,65,25,43,91,52,97,51,14]
         ,[70,11,33,28,77,73,17,78,39,68,17,57]
         ,[91,71,52,38,17,14,91,43,58,50,27,29,48]
         ,[63,66,04,68,89,53,67,30,73,16,69,87,40,31]
         ,[04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]]

acc ps rs = hlpr 0 ps rs
  where hlpr c ps rs
          | c == l    = [v]
          | otherwise = v : hlpr (c+1) ps rs
          where
            l  = (length $ rs) - 1
            x  = rs !! c
            y  = if c > 0 then x + (ps !! (c-1)) else 0
            z  = if c < l then x + (ps !! c) else 0
            v  = if y > z then y else z

ans = maximum $ hlpr 1 (head tri) tri
  where
    hlpr i ps ts
      | i == l    = acc ps (ts !! i)
      | otherwise = hlpr (i+1) (acc ps (ts !! i)) ts
      where
        l = (length ts) - 1
