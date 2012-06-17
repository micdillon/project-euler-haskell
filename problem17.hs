--
-- problem17.hs
--

ones = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", 
         "seventeen", "eighteen", "nineteen"]
tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
thousand = ["onethousand"]

-- 1..99
ninetynine = ones ++ teens ++ (concat (map (\t -> map (\s -> t ++ s) ("" : ones)) tens))

-- 100, 200, 300, ...
hundreds = map (\s -> s ++ "hundred") ones
hundredands = map (\s -> s ++ "hundredand") ones

-- 1..1000
onethousand = (concat (map (\t -> map (\s -> t ++ s) ninetynine) ("" : hundredands)))
              ++ hundreds ++ thousand
