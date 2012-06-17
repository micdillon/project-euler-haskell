--
-- problem15.hs
--
-- (note: give an inductive proof of this)
--
-- -- this problem boils down to choosing 20 from 40
-- -- since to get to the other corner, we must travel
-- -- down 20 squares and left 20 squares and for each
-- -- square we can choose to either go down or left,
-- -- but we must do so exactly 20 times each. thus,
-- -- we can choose 20 moves out of 40 to be left.
--

-- got this answer from mathematica using the 
-- command "40 choose 20".
-- i'll implement the haskell code later for computing 
-- binomial coeffients ;)
ans = 137846528820
