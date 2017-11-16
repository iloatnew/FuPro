--3-3
--a: 
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:_) = Just a

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:b) = Just b

--b:
listEven :: [a] -> Bool
listEven a = if ((length a) `mod` 2 == 0) then True else False
