--FuPro Blatt5 Gruppe9 
-- Li Wentao 198341
-- Kurinchinilavan Thangeswaran 156190 
import Expr
import Control.Applicative
--6-1
--a)

link = Sum [2 :*(Var"x" :^3),5 :*Var"y",Con 2]
recht = Var"z" :^2

--b)

solutions :: [(Int,Int,Int)]
solutions = [(a,b,c)
	| c <-[0..]
	, b <-[0..c^2]
	, a <-[0..c^2]
--	, (exp2store link $ \x -> case x of "x"-> a; "y" -> b) == (exp2store recht $ \"z" -> c)
--	]
--andere Variante:
	-- "let" definate the store
	,let st "x" = a
	     st "y" = b
	     st "z" = c
	,exp2store link st == exp2store recht st
	]

--6-2
bexp = Or [And [(Sum [2 :*(Var"x" :^3),5 :*Var"y",Con 2]) :< (Var"z" :^2),True_], Not (BVar"b")]
type BStore x = x -> Bool
bexp2store :: BExp x -> Store x -> BStore x -> Bool
bexp2store e st bst = case e of True_ -> True
				False_ -> False
				BVar x -> bst x
				--Or e -> or $ map (bexp2store' st bst) e
				--andere Version, ohne bexp2store':
				Or e -> or $ map (\e-> bexp2store e st bst) e
				And e -> and $ map (bexp2store' st bst) e
				Not e -> not (bexp2store e st bst)
				e :< e'-> exp2store e st < exp2store e' st
				e := e' -> exp2store e st == exp2store e' st
				e :<= e' -> exp2store e st <= exp2store e' st
	where bexp2store' st bst e = bexp2store e st bst

b = bexp2store bexp (\x-> case x of "x"->0;"y"->0;"z"->1) (\"b"->False)

--6-3
data Stream a = (:<<) {hd :: a, tl :: Stream a} deriving Show

data Colist a = Colist {split :: Maybe (a,Colist a)} deriving Show
col1 = Colist (Just (999,Colist (Just ( 123,(Colist Nothing)))))

class Dp a where
	drop' :: Int -> a -> a
instance Dp [a] where	
	-- muss [] sowie Nothing betrachtet werden:
	drop' _ [] = []
	drop' 0 xs = xs
	drop' n (x:xs) | n>0 = drop (n-1) xs
instance Dp (Stream a) where
	drop' 0 a = a
	drop' n ((:<<) a b) | n>0 = drop' (n-1) b
instance Dp (Colist a) where
	-- hier auch:
	drop' _ (Colist Nothing) = (Colist Nothing)
	drop' 0 a = a
	drop' n ( Colist ( Just (a,b) ) ) | n>0 = drop' (n-1) b

--6-4
data Bintree a = Empty | Fork a (Bintree a) (Bintree a) deriving Show
data Edge = Links | Rechts deriving Show
type Node = [Edge]

tree = Fork 1 (Fork 2 (Fork 4 Empty Empty) (Fork 5 Empty Empty)) (Fork 3 (Fork 6 Empty Empty) (Fork 7 Empty Empty))
node5 = [Links,Rechts]
node7 = [Rechts,Rechts]
node0 = [Links,Links,Links]

--a)
value :: Node -> Bintree a -> Maybe a
value _ Empty = Nothing
value [] (Fork v l r) = (Just v)
value (a:as) (Fork v l r) = case a of Links -> (value as l); Rechts -> (value as r)

--b)
search :: Eq a => a -> Bintree a -> Maybe Node
search _ Empty = Nothing
search a (Fork v l r) 
	| a==v = (Just [])
--	| otherwise = (fmap (Links:) (search a l)) <|> (fmap (Rechts:) (search a r))
--	eine Variante ohne <|>
	| otherwise = f (search a l) (search a r) where
		-- means a was found in lefttree
		f (Just l) _ = Just $ Links:l		
		f _ (Just r) = Just $ Rechts:r
		f _ _ = Nothing


