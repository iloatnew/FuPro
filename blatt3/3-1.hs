-- 3-1
-- a:
power :: Int -> Int -> Int -> Int
power base expo state = if expo > 0 then power base (expo-1) (state*base) else state

-- b:
summe :: [Int] -> Int -> Int -> Int
summe ls i state = if i< (length ls) then summe ls (i+1) (state+ls !! i) else state
