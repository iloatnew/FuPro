collatz :: Int -> Int
collatz n = case even n of { True -> n `div` 2 ; False -> (3*n + 1)}
-- |的作用： | bool = return
-- | 后 接一个bool，如果true之后，return = 后的值
-- | 前不用 =， 可以直接在函数名称和函数的参数后接|



f :: Int -> Int -> Int
f x y 
	| x==0 && y>50 = y*2 
	| x==0 && y<=50 = y+2 
	| y==0 && x<100 = x*2 
	| otherwise = x+y

a :: Int -> Int -> Int
a b c = case (b,c) of { 
	(0,c) -> case c>50 of {
		True -> c*2; 
		False -> c+2};
	(b,0) -> case b<100 of {
		True -> b*2;
		False -> b+c};
	(b,c) -> b+c }

