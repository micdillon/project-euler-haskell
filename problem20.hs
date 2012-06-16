--
-- problem20.hs
--

import Euler

fact :: Integer -> Integer
fact n = foldl (*) 1 [1..n]

ans = sum $ dgts (fact 100)
