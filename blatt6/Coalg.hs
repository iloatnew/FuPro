{-# LANGUAGE LambdaCase, TypeSynonymInstances #-}

module Coalg where

-- 19.4.2016

import Data.Monoid (Monoid(mempty,mappend))
import Data.List (delete,(\\))
import Control.Applicative (Applicative(pure,(<*>)),liftA2,
			    Alternative(empty,(<|>)))
import Control.Monad 

-- Control.Comonad is not imported. Instead, the Comonad class and related 
-- functions are defined here.

import Painter (Tree(V,F),root,subtrees,height,mapTree,drawTree)

class Contravariant f where
      contramap :: (a -> b) -> f b -> f a

newtype Coreader state a = CR {runCR :: a -> state}

instance Contravariant (Coreader state) where
 	 contramap f (CR h) = CR $ h . f

-- MONADS
         
-- identity monad

newtype Id a = Id {run :: a}

instance Functor Id where fmap f (Id a) = Id $ f a

instance Applicative Id where
         pure = return
         mf <*> m = mf >>= flip fmap m

instance Monad Id where return = Id			
			Id a >>= f = f a						
         
{- reader monads

instance Functor ((->) state) where
 	 fmap f h = f . h

instance Monad ((->) state) where			
         return = const
         (h >>= f) st = f (h st) st

-- writer monads (state is a monoid)
         
instance Functor ((,) state) where			
 	 fmap f (st,a) = (st,f a)
 	 
class Monoid a where mempty :: a
		     mappend :: a -> a -> a
		     
mconcat :: Monoid a => [a] -> a
mconcat = foldr mappend mempty -}

instance Monoid state => Monad ((,) state) where       -- ggf. auszukommentieren	
         return a = (mempty,a)				
         (st,a) >>= f = (st `mappend` st',b) where (st',b) = f a 

-- state monads
         
newtype State state a = State {runS :: state -> (a,state)}

instance Functor (State state) where
 	 fmap f (State h) = State $ (\(a,st) -> (f a,st)) . h

instance Applicative (State state) where
         pure = return
         mf <*> m = mf >>= flip fmap m

instance Monad (State state) where			
         return a = State $ \st -> (a,st)
         State h >>= f = State $ (\(a,st) -> runS (f a) st) . h

-- piggyback state monads

newtype StateT state m a = StateT {runST :: state -> m (a,state)}

instance Monad m => Functor (StateT state m) where
 	 fmap f (StateT h) = StateT $ (\(a,st) -> return (f a,st)) <=< h
	 
instance Monad m => Applicative (StateT state m) where
         pure = return
         mf <*> m = mf >>= flip fmap m

instance Monad m => Monad (StateT state m) where  	
         return a = StateT $ \st -> return (a,st)	        
         StateT h >>= f = StateT $ (\(a,st) -> runST (f a) st) <=< h
	 -- fail _ = StateT $ const mzero		   

instance MonadPlus m => Alternative (StateT state m) where
 	 empty = mzero
 	 (<|>) = mplus
	       
instance MonadPlus m => MonadPlus (StateT state m) where
         mzero = StateT $ const mzero
         StateT g `mplus` StateT h = StateT $ liftM2 mplus g h

 	 
-- COMONADS

infixl 1 <<=

class Functor cm => Comonad cm where 
      extract :: cm a -> a
      (<<=) :: (cm a -> b) -> (cm a -> cm b)

(=<=) :: Comonad cm => (cm b -> c) -> (cm a -> b) -> (cm a -> c)
g =<= f = g . (f <<=)   

duplicate :: Comonad cm => cm a -> cm (cm a)
duplicate = (id <<=)

-- identity comonad

instance Comonad Id where				
         extract = run
         f <<= cm = Id $ f cm

-- reader comonads (state is a monoid)

instance Monoid state => Comonad ((->) state) where 	
         extract g = g mempty				
         (f <<= g) st = f $ g . mappend st 

-- writer comonads

instance Comonad ((,) state) where			
         extract (_,a) = a
         f <<= p@(st,_) = (st,f p)
         
-- costate comonads 

data Costate state a = (:#) {out :: state -> a, final :: state}
							
instance Functor (Costate state) where fmap f (h:#st) = (f . h):#st
     
instance Comonad (Costate state) where	 		
         extract (h:#st) = h st
         f <<= (h:#st) = (f . (:#) h):#st 	


-- ALGEBRAS AND COALGEBRAS


-- F-(co)algebras

data HList a s = HNil | HCons (a,HList a s)		    -- list functor

data TList a = InList (HList a (TList a))		    -- list terms

listH :: HList a [a] -> [a]				    -- a list algebra
listH HNil          = []
listH (HCons (a,s)) = a:listH s

data HStream a s = HStream {hd_ :: a,tl_ :: HStream a s}    -- stream functor

data StreamF a = InStream {finAlg :: HStream a (StreamF a)} -- stream coterms

stream :: [a] -> HStream a [a]				    -- a stream coalgebra
stream (a:s) = HStream {hd_ = a,tl_ = stream s}


-- Natural numbers

data Nat nat = Nat {zero_ :: nat, succ_ :: nat -> nat}

data NatT = Zero | Succ NatT
  
natT :: Nat NatT
natT = Nat {zero_ = Zero, succ_ = Succ}
                   
foldNat :: Nat nat -> NatT -> nat
foldNat alg Zero       = zero_ alg
foldNat alg (Succ nat) = succ_ alg $ foldNat alg nat

data Conat nat = Conat {pred_ :: nat -> Maybe nat}

newtype ConatC = ConatC {predC :: Maybe ConatC}

zero     = ConatC Nothing
infinity = ConatC $ Just infinity

conatT :: Conat ConatC
conatT = Conat predC
  
unfoldConat :: Conat nat -> nat -> ConatC
unfoldConat alg nat = ConatC $ do nat <- pred_ alg nat
				  Just $ unfoldConat alg nat
				  
-- Lists

single a = [a]

-- Numerical list functions

instance Num a => Num [a] where
         (+) = zipWith (+)
	 s*s' = x*head s':tail s*s'+(x:0)*tail s' where x = head s
	 negate s = negate (head s):negate (tail s)
	 abs = id
	 signum _ = 1
	 fromInteger n = fromInteger n:0
	 
instance Fractional a => Fractional [a] where
         recip s = x:negate ((x:0)*tail s)*recip s where x = recip $ head s
	 fromRational r = fromRational r:0

x',y',z' :: Num a => [a]
x' = 0:1
y' = 1:1
z' = 1:0

one = 1:one
sucs = 1:one+sucs
facts = 1:zipWith (*) facts sucs

fibs = 0:fibs+(1:fibs)
fibs1 = 0:tailfibs where tailfibs = 1:fibs1+tailfibs
fibs2 = 0:y'*fibs1+z'
fibs3 = x'*recip (z'-x'-x'*x')

data List x list = List {nil :: list, cons :: x -> list -> list}

foldList :: List x list -> [x] -> list
foldList alg []    = nil alg
foldList alg (x:s) = cons alg x $ foldList alg s

listT :: List x [x]
listT = List {nil = [], cons = (:)}

sumL :: List Int Int
sumL = List {nil = 0, cons = (+)}

list1 = foldList sumL [1..8]
list2 = foldr (+) 0 [1..8]

{- 
instance Functor [ ] where
         fmap = map
      
instance Monad [ ] where				
         return a = [a]
         (>>=) = flip concatMap
      
instance MonadPlus [ ] where
         mzero = []
         mplus = (++)
-}

instance Comonad [ ] where				
         extract = head
         f <<= [] = []
         f <<= s  = f s:(f <<= tail s)

list3 = id <<= [1..5]		-- > [[1,2,3,4,5],[2,3,4,5],[3,4,5],[4,5],[5]]
list4 = length <<= [1..8]	-- > [8,7,6,5,4,3,2,1]
list5 = sum <<= [1..8]		-- > [36,35,33,30,26,21,15,8]

data Colist x list = Colist {split :: list -> Maybe (x,list)}

newtype ColistC x = ColistC {splitC :: Maybe (x,ColistC x)}

colistT :: Colist x (ColistC x)
colistT = Colist splitC

unfoldColist :: Colist x list -> list -> ColistC x
unfoldColist alg list = ColistC $ do (x,list) <- split alg list 
				     Just (x,unfoldColist alg list)
	 
instance Show a => Show (ColistC a) where
	 showsPrec s = f 100 where
	               f 0 _ = id
	               f 1 s = case splitC s of Just (a,s) -> shows a; _ -> id
	               f n s = case splitC s of 
		               Just (a,s) -> case splitC s of 
		                             Nothing -> shows a
					     _ -> shows a . (',':) . f (n-1) s
			       _ -> id

zipWithC f s s' = ColistC {splitC = do (a,s) <- splitC s; (b,s') <- splitC s'
			               Just (f a b,zipWithC f s s')}		      

-- Lists with a position

infixr 4 :@, :#, :&

data ListPos a = (:@) {list :: [a], pos :: Int}	

instance Functor ListPos where fmap f (s:@i) = map f s:@i
      
instance Comonad ListPos where extract (s:@i) = s!!i
         		       f <<= s:@i = map (f . (s:@)) [0..length s-1]:@i
         
prefixSum :: ListPos Int -> Int
prefixSum (s:@0) = s!!0
prefixSum (s:@i) = prefixSum (s:@i-1)+s!!i
         
prefixSum' :: [Int] -> Int -> Int
prefixSum' s 0 = s!!0
prefixSum' s i = prefixSum' s (i-1)+s!!i

list6 = list $ prefixSum <<= [1..8]:@0  
list7 = map (prefixSum' [1..8]) [0..7]	-- >  [1,3,6,10,15,21,28,36]
         
suffixSum :: ListPos Int -> Int
suffixSum (s:@i) = if i >= length s then 0 else s!!i+suffixSum (s:@i+1)

list8 = list $ suffixSum <<= [1..8]:@0  -- >  [36,35,33,30,26,21,15,8]

neighbSum :: ListPos Int -> Int
neighbSum (s:@0) = s!!0+s!!1
neighbSum (s:@i) = if i+1 < length s then s!!(i-1)+s!!i+s!!(i+1)
				     else s!!(i-1)+s!!i

list9 = list $ neighbSum <<= [1..8]:@0  -- >  [3,6,9,12,15,18,21,15]

         
-- Streams
         
data Stream x s = Stream {head_ :: s -> x, tail_ :: s -> s}

infixr 5 :<

data StreamC x = (:<) {hd :: x, tl :: StreamC x}

unfoldStream :: Stream x s -> s -> StreamC x
unfoldStream alg s = head_ alg s :< unfoldStream alg (tail_ alg s)

unfoldStreamF :: Stream x s -> s -> (Int -> x)
unfoldStreamF alg s = \case 0 -> head_ alg s 
			    n -> unfoldStreamF alg (tail_ alg s) $ n-1

streamC :: Stream x (StreamC x)
streamC = Stream hd tl

instance Show a => Show (StreamC a) where
	showsPrec s = f 100 where f 0 _ = id
	 		          f 1 s = shows $ hd s
	 			  f n s = shows (hd s) . (',':) . f (n-1) (tl s)
	 			  
zipW :: (a -> a -> a) -> StreamC a -> StreamC a -> StreamC a
zipW f s s' = f (hd s) (hd s') :< zipW f (tl s) (tl s')

instance Num a => Num (StreamC a) where
         (+) = zipW (+)
	 s*s' = x*hd s':<tl s*s'+(x:<0)*tl s' where x = hd s
	 negate s = negate (hd s):<negate (tl s)
	 abs = id
	 signum _ = 1
	 fromInteger n = fromInteger n:<0
	 
instance Fractional a => Fractional (StreamC a) where
         recip s = x:<negate ((x:<0)*tl s)*recip s where x = recip $ hd s
	 fromRational r = fromRational r:<0

xx,yy,zz :: Num a => StreamC a
xx = 0:<1
yy = 1:<1
zz = 1:<0

blink,blink' :: StreamC Int
blink  = 0 :< blink'
blink' = 1 :< blink

ones' = 1:<ones'
twos  = 2:<twos
sucs' = 1:<ones'+sucs'
facts' = 1:<zipW (*) facts' sucs'

nats n = n:<nats(n+1)

nats1 = 0:<nats1+:ones'

evens s = hd s:<evens (tl $ tl s)

evennats = 0:<oddnats+:ones' 
oddnats  = 1:<oddnats+:twos

repC n = n:<repC n

sumC :: Num a => StreamC (StreamC a) -> StreamC a
sumC ss = hd ss+sumC (tl ss)

su = sumC $ nats 1:<nats 2:<0

fibs4 = 0:<fibs4+(1:<fibs4)
fibs5 = 0:<tailfibs where tailfibs = 1:<fibs5+tailfibs
fibs6 = 0:<yy*fibs6+zz
fibs7 = xx*recip (zz-xx-xx*xx)

(a :< _)!!!0 = a
(_ :< s)!!!n = s!!!(n-1)

(<>) :: (a -> b) -> (a -> c) -> a -> (b,c)
(<>) = liftA2 (,)

pair = ((+1) <> (+2)) 5       		-- > (6,7)

(+:),(*:) :: (Num a,Applicative f) => f a -> f a -> f a

(+:) = liftA2 (+)
(*:) = liftA2 (*)

facts1 = 1:<(nats1+:ones')*:facts1

fibs8 = 1:<fibs8+:(1:<fibs8)

instance Functor StreamC where 
	 fmap f (x:<s) = f x :< fmap f s

instance Applicative StreamC where 
	 pure x = s where s = x :< s
	 (f :< fs) <*> (x :< s) = f x :< (fs <*> s)  		-- zipWith ($)

data ZO = Blink | Blink'

zo :: Stream Int ZO
zo = Stream (\case Blink -> 0; Blink' -> 1)
            (\case Blink -> Blink'; Blink' -> Blink)

streamFun :: Stream x (Int -> x)
streamFun = Stream ($0) (\s n -> s $ n+1)

type StreamFun = (->) Int				    	

instance Monoid Int where mempty = 0
			  mappend = (+)

listF :: StreamFun a -> [a]
listF = flip map [0..]

repeatF :: a -> StreamFun a
repeatF = return

list10 = take 11 $ listF $ repeatF 45

headF :: StreamFun a -> a
headF = extract				

dropF :: Int -> StreamFun a -> StreamFun a
dropF i s = ($i) <<= s				-- ($i) :: StreamFun a -> a

tailF :: StreamFun a -> StreamFun a
tailF = dropF 1				

fby :: StreamFun a -> StreamFun a -> StreamFun a
fby s s' 0 = s $ 0
fby s s' i = s' $ i-1

consF :: a -> StreamFun a -> StreamFun a
consF a as = repeatF a `fby` as

zipWith1,zipWith2 :: (a -> b -> c) -> StreamFun a -> StreamFun b -> StreamFun c
zipWith1 f as bs = do a <- as; b <- bs; return $ f a b
zipWith2 f as bs = consF (f (headF as) $ headF bs) $
		         zipWith2 f (tailF as) $ tailF bs

ones,pnats :: StreamFun Int
ones = repeatF 1
pnats = ones `fby` zipWith2 (+) ones pnats

list11 = take 11 $ listF $ dropF 15 pnats
list12 = take 11 $ listF $ fmap (*3) pnats     
list13 = take 11 $ listF $ zipWith1 (+) pnats pnats
list14 = take 11 $ listF $ zipWith2 (+) pnats pnats

prefixSum1 :: StreamFun Int -> Int -> Int
prefixSum1 s 0 = s 0
prefixSum1 s i = prefixSum1 s (i-1)+s i

-- Streams with a position

type StreamPos = Costate Int				

prefixSum2 :: StreamPos Int -> Int
prefixSum2 (s:#0) = s 0
prefixSum2 (s:#i) = prefixSum2 (s:#(i-1))+s i

-- prefixSum1 = curry $ prefixSum2 . wr2c
-- prefixSum2 = uncurry prefixSum1 . c2wr

list15 = take 8 $ listF $ prefixSum1 pnats
list16 = take 8 $ listF $ out $ prefixSum2 <<= pnats:#0
	 					-- > [1,3,6,10,15,21,28,36]

neighbSum1 :: StreamFun Int -> Int -> Int
neighbSum1 s 0 = s 0+s 1
neighbSum1 s i = s (i-1)+s i+s (i+1)

neighbSum2 :: StreamPos Int -> Int
neighbSum2 (s:#0) = s 0+s 1
neighbSum2 (s:#i) = s (i-1)+s i+s (i+1)

list17 = take 8 $ listF $ neighbSum1 pnats
list18 = take 8 $ listF $ out $ neighbSum2 <<= pnats:#0
	 					-- > [3,6,9,12,15,18,21,24]

-- Infinite trees

type Inftree = (->) [Int]	

type InftreeNode = Costate [Int]

	 									     
-- Binary trees

data Bintree a = Empty | Fork a (Bintree a) (Bintree a) 

instance Functor Bintree where
         fmap f (Fork a left right) = Fork (f a) (fmap f left) $ fmap f right
         fmap _ _ = Empty

instance Comonad Bintree where				
         extract (Fork a _ _) = a
         f <<= Empty                 = Empty
         f <<= t@(Fork _ left right) = Fork (f t) (f <<= left) $ f <<= right

leaf :: a -> Bintree a
leaf a = Fork a Empty Empty

instance Show a => Show (Bintree a) where 
	 showsPrec _ Empty                = id
	 showsPrec _ (Fork a Empty Empty) = shows a 
	 showsPrec _ (Fork a left right)  = shows a . ('(':) . shows left . 
	 				    (',':) . shows right . (')':)

data BtreeAlg a btree = BtreeAlg {empty_ :: btree, 
				  fork :: a -> btree -> btree -> btree}
          
foldBtree :: BtreeAlg a btree -> Bintree a -> btree
foldBtree alg Empty               = empty_ alg
foldBtree alg (Fork a left right) = fork alg a (foldBtree alg left)
					       (foldBtree alg right)	

foldBtreeM :: BtreeAlg a btree -> Bintree a -> Id btree
foldBtreeM alg Empty 	           = return $ empty_ alg
foldBtreeM alg (Fork a left right) = do valL <- foldBtreeM alg left
	        		        valR <- foldBtreeM alg right
	        		        return $ fork alg a valL valR
         
btree1 = Fork 6 (Fork 7 (Fork 11 (leaf 55) $ leaf 33) $ Empty) $ leaf 9
	 -- > 6(7(11(55,33),),9)

sumBA = BtreeAlg 0 (\i j k ->i+j+k)	 

btree2 = foldBtree sumBA btree1	     
btree3 = (run . foldBtreeM sumBA) btree1  	-- > 121	
btree4 = foldBtree sumBA <<= btree1 :: Bintree Int 
btree5 = run . foldBtreeM sumBA <<= btree1 :: Bintree Int 
						-- > 121(106(99(55,33),),9)

-- Trees with finite outdegree

label :: Tree a -> Node -> a
label t [] = root t
label (F _ ts) (i:node) | i < length ts = label (ts!!i) node
label _ _  = error "label"

lab1 = label tree1 [4]  -- >  7

-- getSubtree(t)(node) is the subtree of t whose root is node. 

getSubtree :: Tree a -> Node -> Tree a
getSubtree t [] = t
getSubtree (F _ ts) (i:node) | i < length ts = getSubtree (ts!!i) node
getSubtree _ _  = error "getSubtree"


data TreeAlg a tree trees = TreeAlg {var_:: a -> tree,
				     fun :: a -> trees -> tree, 
				     nil_ :: trees, 
				     cons_ :: tree -> trees -> trees}

foldTree :: TreeAlg a tree trees -> Tree a -> tree
foldTree alg (V a)    = var_ alg a
foldTree alg (F a ts) = fun alg a $ foldTrees alg ts

foldTrees :: TreeAlg a tree trees -> [Tree a] -> trees
foldTrees alg []     = nil_ alg
foldTrees alg (t:ts) = cons_ alg (foldTree alg t) $ foldTrees alg ts

-- preordLA(t) lists the nodes of t in preorder.

preordLA :: TreeAlg a [a] [a]
preordLA  = TreeAlg single (:) [] (++) 

-- postordLA(t) lists the nodes of t in postorder.

postordLA :: TreeAlg a [a] [a]
postordLA  = TreeAlg single (\a s -> s++[a]) [] (++) 

sumA :: Num a => TreeAlg a a a
sumA = TreeAlg id (+) 0 (+) 

arithA :: TreeAlg String Int [Int]
arithA = TreeAlg {fun  = \case "+" -> sum; "*" -> product,
		  var_ = \case "x" -> 5; "y" -> -66; "z" -> 13,
		  nil_ = [], cons_ = (:)}

-- preordT(t) labels each node of t with its position in preordLA(t).

preordT :: Tree a -> Int -> (Tree Int,Int)
preordT (F _ ts) i = (F i us,k) where (us,k)= preordTL ts $ i+1
preordT _ i        = (V i,i+1)

preordTL :: [Tree a] -> Int -> ([Tree Int],Int)
preordTL (t:ts) i = (u:us,k) where (u,j)  = preordT t i; (us,k) = preordTL ts j
preordTL _ i      = ([],i) 

nextPos :: State Int Int
nextPos = State $ \i -> (i,i+1)

preordTM :: Tree a -> State Int (Tree Int)
preordTM (F _ ts) = do i <- nextPos; ts <- preordTLM ts; return $ F i ts
preordTM _        = do i <- nextPos; return $ V i

preordTLM :: [Tree a] -> State Int [Tree Int]
preordTLM (t:ts) = do t <- preordTM t; ts <- preordTLM ts; return $ t:ts
preordTLM _      = return [] 

preordTA :: TreeAlg a (State Int (Tree Int)) (State Int [Tree Int])
preordTA = TreeAlg {var_  = \_   -> do i <- nextPos; return $ V i,
		    fun   = \_ m -> do i <- nextPos; ts <- m; return $ F i ts,
		    nil_  = return [], 
		    cons_ = \m m' -> do t <- m; ts <- m'; return $ t:ts}
		   
tree1,tree2 :: Tree Int
tree1 = F 1 [F 2 [F 2 [V 3,V(-1)],V(-2)],F 4 [V(-3),V 5]]
tree2 = F 11 $ map (\a -> F a [V $ a+1]) [3..11] 

tree3 :: Tree String
tree3 = F "+" [F "*" [V "x",V "y"], V "z"]     

tree4 = foldTree sumA tree2		-- > 146

tree5 = foldTree preordLA tree1

tree6 = foldTree postordLA tree1

tree7 = foldTree arithA tree3		-- > -317

tree8 = fst $ preordT tree2 0
tree9 = fst $ runS (foldTree preordTA tree2) 0	
	 	   -- > F 0 [F 1 [V 2],F 3 [V 4],F 5 [V 6],F 7 [V 8],F 9 [V 10],
	 	   --        F 11 [V 12],F 13 [V 14],F 15 [V 16],F 17 [V 18]]

tree10 = fst $ preordT tree3 0 	
tree11 = fst $ runS (preordTM tree3) 0 	
tree12 = fst $ runS (foldTree preordTA tree3) 0	-- > F 0 [F 1 [V 2,V 3],V 4]

instance Functor Tree where
         fmap = mapTree
         
instance Comonad Tree where				
         extract = root
         f <<= t@(V _)    = V $ f t
         f <<= t@(F _ ts) = F (f t) $ map (f <<=) ts

tree13 = foldTree sumA <<= tree2 	
               -- > F 146 [F 7 [V 4],F 9 [V 5],F 11 [V 6],F 13 [V 7],F 15 [V 8],
               --          F 17 [V 9],F 19 [V 10],F 21 [V 11],F 23 [V 12]]

tree14 = foldTree arithA <<= tree3  -- >  F (-317) [F (-330) [V 5,V (-66)],V 13]

balanced (F _ (t:ts)) = and $ map f ts where f u = height t == height u
balanced _            = True

tree15 = balanced <<= tree3	    -- > F False [F True [V True,V True],V True]

-- Trees with a position

type Node = [Int]

data TreeNode a = (:&) {tree :: Tree a, node :: Node}	     

instance Functor TreeNode where fmap f (t:&node) = mapTree f t:&node

nodeTree :: Tree a -> Node -> Tree Node
nodeTree (V _) node    = V node
nodeTree (F _ ts) node = F node $ zipWith f ts [0..length ts-1]
			 where f t i = nodeTree t $ node++[i]

tree16 = nodeTree tree2 []
            	 -- > F [] [F [0] [V [0,0]],F [1] [V [1,0]],F [2] [V [2,0]],
            	 --         F [3] [V [3,0]],F [4] [V [4,0]],F [5] [V [5,0]],
            	 --         F [6] [V [6,0]],F [7] [V [7,0]],F [8] [V [8,0]]]
                
instance Comonad TreeNode where				
         extract (t:&node) = label t node
         f <<= (t:&node) = mapTree (f . (t:&)) (nodeTree t []):&node        
         
prefixSum3 :: TreeNode Int -> Int
prefixSum3 (t:&[])   = root t
prefixSum3 (t:&node) = prefixSum3 (t:&init node)+label t node
         
prefixSum4 :: Tree Int -> Node -> Int
prefixSum4 t []   = root t
prefixSum4 t node = prefixSum4 t (init node)+label t node

tree17 = tree $ prefixSum3 <<= tree2:&[]
tree18 = mapTree (prefixSum4 tree2) tree16
         -- > F 11 [F 14 [V 18],F 15 [V 20],F 16 [V 22],F 17 [V 24],F 18 [V 26],
         --         F 19 [V 28],F 20 [V 30],F 21 [V 32],F 22 [V 34]]

-- The following version of preordT has been inspired by the numin/numout 
-- example in T. Uustalu, V. Vene. Comonadic functional attribute evaluation, 
-- M. van Eekelen, ed., Trends in Functional Programming 6, Intellect (2007) 
-- 145-162. 

preordTN,nextTN :: TreeNode a -> Int   
preordTN (_:&[]) = 0
preordTN tn      = if fstchild tn then preordTN (parent tn)+1
		   	          else nextTN (prevchild tn)+1
		   	          
-- nextTN(t:&node) is the last position assigned to a proper subtree of t|node.

nextTN tn = if null $ children tn then preordTN tn else nextTN $ lastchild tn

fstchild (t:&node)  = last node == 0
parent (t:&node)    = t:&init node
prevchild (t:&node) = t:&init node++[last node-1]
children (t:&node)  = subtrees $ getSubtree t node
lastchild (t:&node) = t:&node++[lg-1] where lg = length $ children $ t:&node

tree19 = tree $ preordTN <<= tree2:&[]
	 	-- > F 0 [F 1 [V 2],F 3 [V 4],F 5 [V 6],F 7 [V 8],F 9 [V 10],
	 	--        F 11 [V 12],F 13 [V 14],F 15 [V 16],F 17 [V 18]]

tree20 = tree $ nextTN <<= tree2:&[]	
		-- > F 18 [F 2 [V 2],F 4 [V 4],F 6 [V 6],F 8 [V 8],F 10 [V 10],
		--         F 12 [V 12],F 14 [V 14],F 16 [V 16],F 18 [V 18]]

tree21 = tree $ preordTN <<= tree3:&[]	-- > F 0 [F 1 [V 2,V 3],V 4]

tree22 = tree $ nextTN <<= tree3:&[]	-- > F 4 [F 3 [V 2,V 3],V 4]


-- Deterministic automata

data DAut x y state = DAut {delta :: state -> x -> state, beta :: state -> y}

data DAutC x y = DA {deltaC :: x -> DAutC x y, betaC :: y}	

dAutC :: DAut x y (DAutC x y)
dAutC = DAut {delta = deltaC, beta = betaC}

esum,osum :: DAutC Int Bool
esum = DA {deltaC = \x -> if even x then esum else osum, betaC = True}
osum = DA {deltaC = \x -> if even x then osum else esum, betaC = False}

data EO = Esum | Osum deriving Eq

eo :: DAut Int Bool EO
eo = DAut {delta = \case Esum -> f . even; Osum -> f . odd,
	   beta  = (== Esum)}
     where f b = if b then Esum else Osum

behFun :: DAut x y ([x] -> y)
behFun = DAut {delta = \f x -> f . (x:), beta = ($ [])}

unfoldDAutF :: DAut x y state -> state -> [x] -> y
unfoldDAutF alg s = \case [] -> beta alg s
			  x:w -> unfoldDAutF alg (delta alg s x) w

unfoldDAut :: DAut x y state -> state -> DAutC x y
unfoldDAut alg s = DA {deltaC = unfoldDAut alg . delta alg s,
		       betaC = beta alg s}
		       
-- Regular expressions with base sets
	       
data Reg bs reg = Reg {par,seq_ :: reg -> reg -> reg, 
		       iter :: reg -> reg,
		       base :: bs -> reg}
			  
data RegT bs = Par (RegT bs) (RegT bs)| Seq (RegT bs) (RegT bs) | 
	       Iter (RegT bs) | Base bs 
	       deriving (Eq,Show)
	   
regT :: Reg bs (RegT bs)
regT = Reg {par = Par, seq_ = Seq, iter = Iter, base = Base}


{- Monadic automata

class MonadEq m where unit :: a -> m a
		      bind :: (Eq a,Eq b) => m a -> (a -> m b) -> m b
-}		     

data MAut m x y s = MAut {ini :: s, deltaM :: s -> x -> m s, betaM :: s -> y}

reach :: Monad m => MAut m x y s -> [x] -> s -> m s
reach _ [] s      = return s
reach alg (x:w) s = deltaM alg s x >>= reach alg w

obs :: (Functor m,Monad m) => MAut m x y s -> s -> [x] -> m y
obs alg s w = fmap (betaM alg) $ reach alg w s

beh :: (Functor m,Monad m) => MAut m x y s -> [x] -> m y
beh alg = obs alg $ ini alg


-- Nondeterministic automata

type NDAut = MAut [ ]

{- data Set a = S {unS :: [a]} deriving Eq		-- powerset monad

instance MonadEq Set where unit a = S [a]
			   S s `bind` f = S $ mkSet $ concatMap (unS . f) s
				     
mkSet :: Eq a => [a] -> [a]
mkSet = foldl addS []

addS :: Eq a => [a] -> a -> [a]
addS s@(a:t) b = if a == b then s else a:addS t b
addS _ a       = [a]
-}


-- Probabilistic automata

data Dist a = D {runD :: [(a,Float)]}			-- distribution monad

instance Functor Dist where
         fmap f (D d) = D [(f a,p) | (a,p) <- d]
 	 
instance Applicative Dist where
         pure = return
         mf <*> m = mf >>= flip fmap m
         
instance Monad Dist where 
         return a = D [(a,1)]
	 D d >>= f = D [(b,p*q) | (a,p) <- d, (b,q) <- runD $ f a]
	 fail _ = D []

plusD :: Eq a => Dist a -> Dist a -> Dist a
D d `plusD` D d' = D $ foldl addD d d'

addD :: Eq a => [(a,Float)] -> (a,Float) -> [(a,Float)]
addD (ap@(a,p):s) bq@(b,q) = if a == b then (a,p+q):s else ap:addD s bq
addD _ ap                  = [ap]

-- Examples from Erwig, Kollmansberger, Probabilistic Functional Programming
-- in Haskell

float :: RealFloat a => Int -> a
float = fromInteger . toInteger

uniform :: [a] -> Dist a
uniform s = D [(a,1/total) | a <- s] where total = float $ length s

dice = [1..6]

two6 :: MAut Dist () Bool [Int]
two6 = MAut [] (\s -> const $ uniform [s++[n] | n <- dice])
	       (\s -> length (filter (== 6) s) >= 2)

data Marble = R | G | B deriving Eq

rgb :: MAut Dist () Bool ([Marble],[Marble])
rgb = MAut ([R,R,G,G,B],[]) f ((== [R,G,B]) . snd)
      where f (jar,drawn) _ = uniform [(delete m jar,drawn++[m]) | m <- jar]

doors = [1..3]

data HallState = HS {prize,chosen,opened :: Int} deriving Eq

data Action = Hide | Choose | Open | Switch Bool

game :: MAut Dist Action Bool HallState
game = MAut (HS {prize = undefined, chosen = undefined, opened = undefined})
            (\s -> uniform .
	                \case 
	                  Hide -> [s{prize=d} | d <- doors]
		          Choose -> [s{chosen=d} | d <- doors]
		          Open -> [s{opened=d} | d <- doors\\[prize s,chosen s]]
		          Switch True 
			    -> [s{chosen=d} | d <- doors\\[opened s,chosen s]]
		          _ -> [s])
            (\s -> chosen s == prize s)

dexa :: Int -> [(Bool,Float)]
dexa 1 = runD $ beh two6 $ replicate 4 ()
         -- > [(False,0.868048),(True,0.13194445)]
dexa 2 = runD $ beh rgb $ replicate 3 ()
         -- > [(False,0.93333346),(True,6.666667e-2)]
dexa 3 = runD $ beh game [Hide,Choose,Open,Switch True] 
         -- > [(False,0.33333334),(True,0.6666667)]
dexa _ = runD $ beh game [Hide,Choose,Open,Switch False]
         -- > [(True,0.33333334),(False,0.6666667)]

         
-- HORN AND CO-HORN CLAUSES


sorted :: Ord a => [a] -> Bool
sorted (x:y:s) = x <= y && sorted (y:s)
sorted _       = True

unsorted :: [Int] -> Bool
unsorted s = s `elem` unsorted' 
	     where unsorted' = do s'@(x:y:s) <- sequence $ replicate 3 [1..11]
				  guard $ x > y || unsorted (y:s)
				  [s']

