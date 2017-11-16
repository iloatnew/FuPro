--FuPro Blatt3 Gruppe9 
-- Li Wentao 198341
-- Kurinchinilavan Thangeswaran 156190 

-- 3-1
-- notation: 定义本地function： where
-- 用where时不用再次声明 :: type
-- 可以直接写方程
--a:
--power:: Int -> Int -> Int
--power base expo = lop base expo 1 where
--lop:: Int -> Int -> Int -> Int 不一定需要
--lop _ 0 state = state 
--lop a b state = lop a (b-1) (state*a)

-- 用where定义时候，lop由于是power的子函数，所以lop也可以识别 base，尽管base不是lop的参数
-- where 直接写在后边不行， 换行再Tab可以，为什么？——因为Tab表示上一个函数未完结，如果直接换行，则后边的无法和where联系起来
power:: Int -> Int -> Int
power base expo = lop 1 expo where
	lop state expo = if expo > 0 then lop (state * base) (expo -1) else state

--b:
summe:: [Int] -> Int
summe x = loop x 0 where
loop:: [Int] -> Int -> Int 
loop (a:as) state = loop as (state + a)
loop [] state = state

--3-2
--a:
-- take:
-- take 0 _ = [] 
-- take n (x:xs) = x:(take n-1 xs)

{-
take 2 $ tail [2,3,5,4,1]
--	first the $
take 2 (tail [2,3,5,4,1])
--	then tail
take 2 [3,5,4,1]
--	take rekursiv berechnen:
3:(take (2-1) [5,4,1])
3:5:(take (1-1) [4,1])
3:5:[] 
[3,5]

--b:
-- drop 0 xs = xs
-- drop n (x:xs) = drop (n-1) xs

--	same with head, head shall also be rekursiv 
head $ drop 2 [1,4,5,3,2]
head (drop 2 [1,4,5,3,2])
head (drop 1 [4,5,3,2])
head (drop 0 [5,3,2])
head [5,3,2]
5

--c:
--foldl:
--foldl f state [] = state
--foldl f state (x:xs) = foldl f (f state x) xs

foldl (-) 8 [5,2]
foldl (-) ((-) 8 5) [2]
foldl (-) ((-) ((-) 8 5) 2) []
(-) ((-) 8 5) 2
(-) 3 2
1

--d:
--foldr
--foldr f state [] = state
--foldr f state (x:xs) = f x $ foldr f state xs
--remember: $ first

foldr (-) 8 [5,2]
(-) 5 $ (foldr (-) 8 [2])
(-) 5 $ ((-) 2 $ (foldr (-) 8 []))
(-) 5 $ ((-) 2 $ 8)
(-) 5 $ -6
11
-}

--3-3
--a: 
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:_) = Just a

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:b) = Just b

--b:
--problem: not allowed to use other function in this question
listEven :: [a] -> Bool
listEven [] = True --for list with no element
listEven [_] = False --for list with just one element
listEven (_:_:xs) = listEven xs --for all the rest



--listEven a = if ((length a) `mod` 2 == 0) then True else False
