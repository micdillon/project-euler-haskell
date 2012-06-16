--
-- problem9.hs
--

-- definitely the brute force method, but it got the right numbers in one line of code!
-- [(a,b,c)| a<-[1..998], b<-[1..998], c<-[1..998], a < b, b < c, a+b+c == 1000, a*a + b*b == c*c]
-- answer: a*b*c = 200*375*425 = 31875000

cnt :: Integer -> Integer -> (Integer,Integer,Integer)
cnt a b
  | a*a + b*b == c*c = (a,b,c)
  | c > b            = cnt a (b+1)
  | otherwise        = cnt (a+1) (a+2)
    where c = 1000-(a+b)
