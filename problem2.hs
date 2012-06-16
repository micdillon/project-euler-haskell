--
-- problem2.hs
--

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

sum [x| x <- map fib [1..33], x `mod` 2 == 0]
