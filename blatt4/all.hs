import Functions
--4-1
double :: [Int] -> [Int]
double list = map (*2) list
----------------------------------------------------------
funs :: Int -> [Int]
funs x = map ($ x) [(+1),(*2),(^2)]
----------------------------------------------------------
toUnicode :: String -> [Int]
toUnicode str = map fromEnum str

--4-2
listPlus :: [a] -> Maybe a -> [a]
listPlus list Nothing = list
listPlus list (Just a) = ((++) list [a])

catMaybes :: [Maybe a] -> [a]
catMaybes list = foldl listPlus [] list
-----------------------------------------------------------
data Color = Red | Green | Blue
data Counter = Counter { red, green, blue :: Int} deriving Show

couPlus :: Counter -> Color -> Counter
couPlus (Counter r1 g1 b1) color = case color of{
				Red -> Counter (r1+1) g1 b1;
				Green -> Counter r1 (g1+1) b1;
				Blue -> Counter r1 g1 (b1+1)
				}

count :: [Color] -> Counter
count clist = foldl couPlus (Counter 0 0 0) clist

--4-3
divisors :: Int -> [Int]
divisors a = [i|i <- [1..a],
                gcd a i == i]
-----------------------------------------------------------
codes :: [[(Char,Int)]]
codes = [zipWithInds code | code <- perms "zweivrschx",
                            let [z,w,e,i,v,r,s,c,h] = map (getIndex code) "zweivrsch",
                            1000*(z+v)+100*(w+i)+10*(e+e)+i+r ==
                            10000*s+1000*e+100*c+10*h+s]

getIndex :: Eq a => [a] -> a -> Int
getIndex s a = fromJust $ lookup a $ zipWithInds s

zipWithInds :: [a] -> [(a,Int)]
zipWithInds s = zip s $ indices s

-----------------------------------------------------------
solutions :: [(Int, Int, Int)]
solutions = [(x,y,z) | x<-[0..100],y<-[0..100],z <-[0..100],
                       2*(x^3) + 5*y + 2 == z^2]

--4-4
evens::[Int]
evens = [x| x<-[1..], even x]
-----------------------------------------------------------
iterate (*2) 3 !! 2  ~>
3: iterate (*2) 3 !! 2 ~>
3: 6 : iterate (*2) 6 !! 2 ~>
3: 6: 12: iterate (*2) 12 !! 2 ~>
6: 12: iterate (*2) 12 !! 1 ~>
12: iterate (*2) 12 !! 1 ~>
12

