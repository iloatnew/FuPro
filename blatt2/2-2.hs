div' :: Int -> Int -> Maybe Int
div' x 0 = Nothing
div' x y = Just (x `div` y)
    
-- Maybe Int 意味着， 有可能结果不是Int，也就是除0的情况，结果不是自然数（Nothing）
-- 加入Just之后 ，就把Int变成了Maybe Int格式
-- x 0这一行必须在上面 因为haskell从上往下 这样才能保证第三行x y中，y不会为0

