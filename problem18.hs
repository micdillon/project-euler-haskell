--
-- problem18.hs
--
-- strategy: suppose we know the max path for n rows, then
-- select the largest of the two choices. now, check the paths
-- that end in values of row n+1 that are larger than the 
-- one already picked. these paths are the only ones that can
-- possibly be larger than the current one.
--

triangle = [[3]
           ,[7,4]
           ,[2,4,6] 
           ,[3,5,9,8]]

bigTri = [[75]
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


-- get all possible paths from the given 
-- node back to the root
rpaths (r,c) a ts
  | r == 0    = [a+v]
  | otherwise = left ++ right
  where 
    l     = (length $ (ts !! r)) - 1
    v     = (ts !! r) !! c
    left  = if c > 0 then rpaths (r-1,c-1) (a+v) ts else []
    right = if c < l then rpaths (r-1,c) (a+v) ts else []

test_rpath = rpaths (4,2) 0 bigTri

indicesOver v xs = hlpr 0 v xs
  where 
    hlpr i v [] = []
    hlpr i v (x:xs)
      | x > v     = i : hlpr (i+1) v xs
      | otherwise = hlpr (i+1) v xs

nextMax (r,c) ps ts
  | null cs   = (c,v)
  | otherwise = maximumBy () cs
  where 
    rs = ts !! r
    v  = rs !! c
    cs = map (\i -> (i, maximum $ rpaths (r,i) 0 ts)) (indicesOver v rs)

ans = "doof"
