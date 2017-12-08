{-# LANGUAGE LambdaCase, TypeFamilies #-}

module Expr where

-- 4.6.2017

import Data.Maybe
import Control.Exception
import Control.Applicative (Applicative(pure,(<*>)),Alternative(empty,(<|>)))
import Control.Monad
import Painter (readFileContinue,readFileAndDo,fold2,mkInt,Rules,reduce,
	        Tree(V,F),root,mkTree,insert,update,updList)
import Coalg (TreeAlg(TreeAlg,var_,fun,nil_,cons_),foldTree,State(State),runS,
	      StateT(StateT),runST)
     		  
-- MONAD TRANSFORMER

newtype MaybeT m a = MaybeT {runMT :: m (Maybe a)}

instance Monad m => Monad (MaybeT m) where
         return = MaybeT . return . Just
 	 m >>= f = MaybeT $ do ma <- runMT m
 			       case ma of Just a -> runMT $ f a
 			       		  _ -> return Nothing 	
			       		        
instance Monad m => MonadPlus (MaybeT m) where
 	 mzero = MaybeT $ return Nothing
         m `mplus` m' = MaybeT $ do ma <- runMT m
 			            if isJust ma then return ma else runMT m'

instance Monad m => Functor (MaybeT m) where fmap f = (>>= return . f)
 	 
instance Monad m => Applicative (MaybeT m) where pure = return
         					 mf <*> m = mf >>= flip fmap m

instance Monad m => Alternative (MaybeT m) where empty = mzero
 	 					 (<|>) = mplus

single :: a -> [a]
single a = [a]

newtype ListT m a = ListT {runLT :: m [a]}

instance Monad m => Monad (ListT m) where
         return = ListT . return . single
 	 m >>= f = ListT $ do mas <- runLT m
 	 		      mass <- mapM (runLT . f) mas
 			      return $ concat mass
			       		        
instance Monad m => MonadPlus (ListT m) where
 	 mzero = ListT $ return []
         m `mplus` m' = ListT $ do [mas,mas'] <- mapM runLT [m,m']
 			           return $ mas++mas'

instance Monad m => Functor (ListT m) where fmap f = (>>= return . f)
 	 
instance Monad m => Applicative (ListT m) where pure = return
         					mf <*> m = mf >>= flip fmap m

instance Monad m => Alternative (ListT m) where empty = mzero
 	 					(<|>) = mplus

class MonadTrans t where type M t :: * -> *
			 lift  :: Monad m => m a -> t m a
			 lift' :: Monad m => M t a -> t m a			 
		        
instance MonadTrans MaybeT where 
	            type M MaybeT = Maybe 
	            lift  = MaybeT . liftM Just    -- :: m a -> MaybeT m a
		    lift' = MaybeT . return        -- :: Maybe a -> MaybeT m a
		 
instance MonadTrans ListT where 
		    type M ListT = [ ] 
		    lift  = ListT . liftM single   -- :: m a -> ListT m a
		    lift' = ListT . return	   -- :: [a] -> ListT m a
 	 					
instance MonadTrans (StateT state) where
		    type M (StateT state) = State state
                    lift m = StateT $ \st -> do a <- m; return (a,st)
                    		         	   -- :: m a -> StateT state m a
                    lift' (State f) = StateT $ return . f  
                    		         -- :: State state a -> StateT state m a
 	 					 
-- EXPRESSIONS

data Exp x = Con Int | Var x | Sum [Exp x] | Prod [Exp x] | Exp x :- Exp x | 
	     Exp x :/ Exp x | Int :* Exp x | Exp x :^ Int deriving (Eq,Show)
	    
instance Functor Exp where 
         fmap f = \case Con i   -> Con i
         		Var x   -> Var $ f x
         		Sum es  -> Sum $ map (fmap f) es
         		Prod es -> Prod $ map (fmap f) es
         		e :- e' -> fmap f e :- fmap f e'
         		e :/ e' -> fmap f e :/ fmap f e'
         		i :* e  -> i :* fmap f e
         		e :^ i  -> fmap f e :^ i
	    
instance Monad Exp where 					-- Exp monad
         return = Var
         e >>= f = case e of Con i   -> Con i
         		     Var x   -> f x
                             Sum es  -> Sum $ map (>>= f) es
                             Prod es -> Prod $ map (>>= f) es
                             e :- e' -> (e >>= f) :- (e' >>= f)
                             e :/ e' -> (e >>= f) :/ (e' >>= f)
                             i :* e  -> i :* (e >>= f)
                             e :^ i  -> (e >>= f) :^ i

instance Applicative Exp where
         pure = return
         ef <*> e = ef >>= flip fmap e
	    
data BExp x = True_ | False_ | BVar x | Or [BExp x] | And [BExp x] |
              Not (BExp x) | Exp x :< Exp x | Exp x := Exp x | Exp x :<= Exp x
              deriving (Eq,Show)

data ExpAlg x exp = ExpAlg {con       :: Int -> exp,		-- signature
			    var       :: x -> exp,
			    sum_,prod :: [exp] -> exp,
			    sub,div_  :: exp -> exp -> exp,
			    scal      :: Int -> exp -> exp,
			    expo      :: exp -> Int -> exp}

foldExp :: ExpAlg x exp -> Exp x -> exp
foldExp alg = \case Con i   -> con alg i
		    Var x   -> var alg x
		    Sum es  -> sum_ alg $ map (foldExp alg) es
		    Prod es -> prod alg $ map (foldExp alg) es
		    e :- e' -> sub alg (foldExp alg e) $ foldExp alg e'
		    e :/ e' -> div_ alg (foldExp alg e) $ foldExp alg e'
		    i :* e  -> scal alg i $ foldExp alg e
		    e :^ i  -> expo alg (foldExp alg e) i

termAlg :: ExpAlg String (Exp String)
termAlg = ExpAlg Con Var Sum Prod (:-) (:/) (:*) (:^) 

storeAlg :: ExpAlg x (Store x -> Int)      -- (->) (Store x) is a reader functor
storeAlg = ExpAlg {con  = const,
		   var  = flip ($),
		   sum_ = \es -> do is <- sequence es; return $ sum is,
		   prod = \es -> do is <- sequence es; return $ product is,
		   sub  = \e e' -> do i <- e; k <- e'; return $ i-k,
		   div_ = \e e' -> do i <- e; k <- e'; return $ i`div`k,
		   scal = \i e ->  do k <- e; return $ i*k,
		   expo = \e i ->  do k <- e; return $ k^i}		   

exp2store :: Exp x -> Store x -> Int	   	-- exp2store = foldExp storeAlg
exp2store e st = case e of Con i   -> i
	                   Var x   -> st x
	                   Sum es  -> sum $ map eval es
	                   Prod es -> product $ map eval es
	                   e :- e' -> eval e - eval e'
	                   e :/ e' -> eval e `div` eval e'
	                   i :* e  -> i * eval e
	                   e :^ i  -> eval e ^ i
	         where eval = flip exp2store st	  

exp2storeM :: Exp x -> Store x -> Maybe Int	  
exp2storeM e st = case e of Con i   -> Just i
	                    Var x   -> Just $ st x
	                    Sum es  -> do is <- mapM eval es; Just $ sum is
	                    Prod es -> do is <- mapM eval es; Just $ product is
	                    e :- e' -> do i <- eval e; k <- eval e'; Just $ i-k
	                    e :/ e' -> do i <- eval e; k <- eval e'
	                    		  guard $ k /= 0; Just $ i `div` k
	                    i :* e  -> do k <- eval e; Just $ i*k
	                    e :^ i  -> do k <- eval e; Just $ k^i
	          where eval = flip exp2storeM st	  

exp2storeR :: Exp x -> Store x -> Int		-- exp2storeR = exp2store
exp2storeR = \case 
	      Con i   -> return i
	      Var x   -> ($x)
	      Sum es  -> do is <- mapM exp2storeR es; return $ sum is
	      Prod es -> do is <- mapM exp2storeR es; return $ product is
	      e :- e' -> do i <- exp2storeR e; k <- exp2storeR e'; return $ i-k
	      i :* e  -> do k <- exp2storeR e; return $ i*k
	      e :/ e' -> do i <- exp2storeR e; k <- exp2storeR e'
	      		    return $ i`div`k
	      e :^ i  -> do k <- exp2storeR e; return $ k^i
	      
exp2storeRT :: Exp x -> MaybeT ((->) (Store x)) Int	    
exp2storeRT = \case 
	       Con i   -> return i
	       Var x   -> lift ($x)
	       Sum es  -> do is <- mapM exp2storeRT es; return $ sum is
	       Prod es -> do is <- mapM exp2storeRT es; return $ product is
	       e :- e' -> do i <- exp2storeRT e; k <- exp2storeRT e'
	       		     return $ i-k
	       e :/ e' -> do i <- exp2storeRT e; k <- exp2storeRT e'
	       		     guard $ k /= 0; return $ i`div`k
	       i :* e  -> do k <- exp2storeRT e; return $ i*k
	       e :^ i  -> do k <- exp2storeRT e; return $ k^i
	      
exp2storeIO :: (Eq x,Read x,Show x) => String -> Exp x -> MaybeT IO Int	    
exp2storeIO file = \case 
	            Con i   -> return i
	            Var x   -> do store <- lift $ readFileContinue file [] 
	            				$ return . read
	            		  let val = lookup x store
	                          lift $ putStrLn $ show x ++
	                                   case val of Just i -> " = " ++ show i
	                          		       _ -> " is undefined"
	                          lift' val
	            Sum es  -> do is <- mapM eval es; return $ sum is
	            Prod es -> do is <- mapM eval es; return $ product is
	            e :- e' -> do i <- eval e; k <- eval e'; return $ i-k
	      	    e :/ e' -> do i <- eval e; k <- eval e'
	       		          if k /= 0 then return $ i`div`k
	       		          else do lift $ putStrLn "divide by zero?"
	       		                  mzero
	            i :* e  -> do k <- eval e; return $ i*k
	            e :^ i  -> do k <- eval e; return $ k^i
	           where eval = exp2storeIO file

-- assembler commands
	 
data StackCom x = Push Int | Load x | Add Int | Mul Int | Sub | Up | Div
	          deriving Show

type Estate x = ([Int],Store x)

executeCom :: StackCom x -> Estate x -> Estate x
executeCom (Push a) (stack,store) = (a:stack,store)
executeCom (Load x) (stack,store) = (store x:stack,store) 
executeCom (Add n) st             = executeOp sum n st
executeCom (Mul n) st             = executeOp product n st
executeCom Sub st                 = executeOp (foldl1 (-)) 2 st
executeCom Up st                  = executeOp (foldl1 (^)) 2 st
						
executeOp :: ([Int] -> Int) -> Int -> Estate x -> Estate x
executeOp f n (stack,store) = (f (reverse as):bs,store)
                              where (as,bs) = splitAt n stack

execute :: [StackCom x] -> Estate x -> Estate x
execute = flip $ foldl $ flip executeCom

codeAlg :: ExpAlg x [StackCom x]
codeAlg = ExpAlg {con = \i     -> [Push i],
		  var = \x     -> [Load x],
		  sum_ = \cs   -> concat cs++[Add $ length cs],
		  prod = \cs   -> concat cs++[Mul $ length cs],
		  sub = \c c'  -> c++c'++[Sub],
		  div_ = \c c' -> c++c'++[Div],
		  scal = \i c  -> Push i:c++[Mul 2],
		  expo = \c i  -> c++[Push i,Up]}

exp2code :: Exp x -> [StackCom x]		-- exp2code = fold^codeAlg
exp2code = \case Con i   -> [Push i]
		 Var x   -> [Load x]
		 Sum es  -> concatMap exp2code es++[Add $ length es]
		 Prod es -> concatMap exp2code es++[Mul $ length es]
		 e :- e' -> exp2code e++exp2code e'++[Sub]
		 e :/ e' -> exp2code e++exp2code e'++[Div]
		 i :* e  -> Push i:exp2code e++[Mul 2]
		 e :^ i  -> exp2code e++[Push i,Up]
	
type Store x = x -> Int
                        
exp2text :: Exp String -> Store String -> (String,Int)
					    -- exp2text e st is a writer functor
exp2text e st = case e of Con i    -> ("",i)
		          Var x    -> out $ st x
		          Sum es   -> do is <- mapM comp es; out $ sum is
		          Prod es  -> do is <- mapM comp es; out $ product is
		          e1 :- e2 -> do i <- comp e1; k <- comp e2; out $ i-k
		          e1 :/ e2 -> do i <- comp e1; k <- comp e2 
		                         out $ i`div`k
		          i :* e'  -> do k <- comp e'; out $ i*k
		          e' :^ i  -> do k <- comp e'; out $ k^i
             where out i = ("The value of "++showExp e++" is "++show i++".\n",i)
	           comp = flip exp2text st
                        
treeAlg :: ExpAlg String (Tree String)			
treeAlg = ExpAlg {con  = \i    -> int i,
		  var  = \x    -> V x,
		  sum_ = \ts   -> F "Sum" ts,
		  prod = \ts   -> F "Prod" ts,
		  sub  = \t t' -> F "-" [t,t'],
		  div_ = \t t' -> F "/" [t,t'],
		  scal = \i t  -> F "*" [int i,t],
		  expo = \t i  -> F "^" [t,int i]}	
	  where int i = F (show i) []

exp2tree :: Exp String -> Tree String		  -- exp2tree = fold^treeAlg
exp2tree = \case Con i   -> int i
	         Var x   -> V x
	         Sum es  -> F "Sum" $ map exp2tree es
	         Prod es -> F "Prod" $ map exp2tree es
	         e :- e' -> F "-" [exp2tree e, exp2tree e']
	         e :/ e' -> F "/" [exp2tree e, exp2tree e']
	         i :* e  -> F "*" [int i, exp2tree e]
	         e :^ i  -> F "^" [exp2tree e, int i]
	   where int i = F (show i) []

expA :: Store String -> TreeAlg String Int [Int]
expA st = TreeAlg {var_ = st, fun = \case "Sum" -> sum 
				          "Prod" -> product
				          "-" -> \[i,k] -> i-k
				          "/" -> \[i,k] -> i`div`k
				          "*" -> \[i,k] -> i*k
				          "^" -> \[i,k] -> i^k
				          i -> const $ read i,
	           nil_ = [], cons_ = (:)}
	     
evalTree :: Tree String -> Store String -> Int
evalTree = flip $ foldTree . expA	      -- evalTree . exp2tree = exp2store

-- symbolic differentiation with subsequent normalization

diff :: Eq x => Exp x -> x -> Exp x
diff e x = case e of Con _   -> zero
		     Var y   -> if x == y then one else zero
		     Sum es  -> reduceE $ Sum $ map d es
		     Prod es -> reduceE $ Sum $ zipWith f [0..] es
		                where f i = reduceE . Prod . updList es i . d
		     e :- e' -> reduceE $ d e :- d e'
		     i :* e  -> reduceE $ i :* d e
		     e :^ i  -> reduceE $ i :* reduceE (Prod [d e,e:^(i-1)])
           where d = flip diff x

-- stepwise differentiation

diff1 = reduce "diff" [diffStep] 

diffStep,redStep :: Rules String

diffStep (F "$" [F "diff" [V x],e]) = 
        case e of F "Con" [t] | isJust i 
   			        -> Just $ mkCon 0 where i = parse int $ root t
                  F "Var" [V y] -> Just $ mkCon $ if x == y then 1 else 0
                  F "Sum" es    -> Just $ F "Sum" $ map diff es
                  F "Prod" es   -> Just $ F "Sum" $ zipWith f [0..] es
                  		   where f i = F "Prod" . updList es i . diff
                  F ":-" [e,e'] -> Just $ F ":-" [diff e,diff e']
                  F ":*" [t,e] | isJust $ parse int $ root t
	                        -> Just $ F ":*" [t,diff e]
                  F ":^" [e,t] | isJust i 
		    	        -> Just $ F ":*" [t,F "Prod" [diff e,e']]
		                   where i = parse int $ root t
				         e' = F ":^" [e,mkInt $ fromJust i-1]
		  _ -> Nothing
        where diff e = F "$" [F "diff" [V x],e]
diffStep _ = Nothing  

redStep e = case e of F "Sum" es | any (== zero) es
				   -> Just $ F "Sum" $ filter (/= zero) es 
				      where zero = mkCon 0
                      F "Sum" [e]  -> Just e
                      F "Prod" es | any (== one) es 
                        	   -> Just $ F "Sum" $ filter (/= one) es 
                        	      where one = mkCon 1
		      F "Prod" [e] -> Just e
                      F ":*" [t,F ":*" [u,e]] | isJust i && isJust j 
			      -> Just $ F ":*" [mkInt $ fromJust i*fromJust j,e]
				 where [i,j] = map (parse int . root) [t,u]
                      F ":^" [F ":^" [e,t],u] | isJust i && isJust j
			      -> Just $ F ":^" [e,mkInt $ fromJust i*fromJust j] 
			         where [i,j] = map (parse int . root) [t,u]
		      _ -> Nothing
                 
mkCon :: Int -> Tree String
mkCon i = F "Con" [mkInt i]

zero = Con 0
one  = Con 1

-- stepwise evaluation

evalStep :: Store String -> Rules String
evalStep st (F "exp2store" [e]) = 
            case e of F "Con" [t] | isJust i -> Just $ mkInt $ fromJust i
	                                        where i = parse int $ root t
	              F "Var" [V x]          -> Just $ mkInt $ st x
	              F "Sum" es             -> Just $ F "+" $ map eval es
	              F "Prod" es            -> Just $ F "*" $ map eval es
	              F ":-" [e,e']          -> Just $ F "-" [eval e,eval e']
	              F ":/" [e,e']          -> Just $ F "/" [eval e,eval e']
	              F ":*" [t,e] | isJust i 
	                             -> Just $ F "*" [mkInt $ fromJust i,eval e]
	             		        where i = parse int $ root t
	              F ":^" [e,t] | isJust i 
	                             -> Just $ F "^" [eval e,mkInt $ fromJust i]
	             		        where i = parse int $ root t
	              _ -> Nothing
	    where eval e = F "exp2store" [e]
evalStep _ _ = Nothing

-- output

showExp :: Exp String -> String
showExp e = showsPrec 0 e "" where 
            showsPrec :: Int -> Exp String -> String -> String
            showsPrec _ (Con i)   = shows i
            showsPrec _ (Var x)   = (x++)
            showsPrec p (Sum es)  = enclose (p>0) $ showMore 0 '+' es
            showsPrec p (Prod es) = enclose (p>1) $ showMore 1 '*' es
            showsPrec p (e :- e') = enclose (p>0) $ showsPrec 0 e . ('-':) . 
                                	            showsPrec 1 e' 
            showsPrec p (e :/ e') = enclose (p>1) $ showsPrec 1 e . ('/':) . 
                                	            showsPrec 2 e' 
            showsPrec p (i :* e)  = enclose (p>1) $ shows i . ('*':) .
         					    showsPrec 1 e 
            showsPrec p (e :^ i)  = enclose (p>2) $ showsPrec 2 e . ('^':) . 
         					    shows i
		
            enclose :: Bool -> (String -> String) -> String -> String
            enclose b f = if b then ('(':) . f . (')':) else f
			
            showMore :: Int -> Char -> [Exp String] -> String -> String
            showMore p op (e:es) = foldl f (showsPrec p e) es where
	                           f state e = state . (op:) . showsPrec p e    	           

-- normalization

type Rstate x = (Int,[Exp x],Exp x -> Int)

updState :: Eq x => Rstate x -> Exp x -> Int -> Rstate x
updState (c,bases,f) e i = (c,insert e bases,update f e $ f e+i)

applyL :: ([a] -> a) -> [a] -> a
applyL _ [a] = a
applyL f as  = f as

reduceE :: Eq x => Exp x -> Exp x
reduceE =       \case e :- e' -> reduceE $ Sum [e,(-1):*e']
		      i :* Con j -> Con $ i*j
		      0 :* e -> zero
		      1 :* e -> reduceE e
		      i :* (j :* e) -> (i*j) :* reduceE e
		      i :* e -> i :* reduceE e
		      Con i :^ j -> Con $ i^j
		      e :^ 0 -> one
		      e :^ 1 -> reduceE e
		      (e :^ i) :^ j -> reduceE e :^ (i*j)
		      e :^ i -> reduceE e :^ i
		      Sum es -> case f es of (c,[]) -> Con c
				    	     (0,es) -> applyL Sum es
				             (c,es) -> applyL Sum $ Con c:es
		      Prod es -> case g es of (c,[]) -> Con c
		     		     	      (1,es) -> applyL Prod es
		     		     	      (c,es) -> c :* applyL Prod es
		      e -> e
      where f es = (c,map summand bases) where
                   (c,bases,scal) = foldl trans (0,[],const 0) $ map reduceE es
                   summand e = if i == 1 then e else i :* e where i = scal e
                   trans state@(c,bases,scal) = \case Con 0 -> state
	     					      Con i -> (c+i,bases,scal)
	     					      i:*e -> updState state e i
	     					      e -> updState state e 1
            g es = (c,map factor bases) where
                   (c,bases,expo) = foldl trans (1,[],const 0) $ map reduceE es
                   factor e = if i == 1 then e else e :^ i where i = expo e
                   trans state@(c,bases,expo) = \case Con 1 -> state
	     					      Con i -> (c*i,bases,expo)
	     					      e:^i -> updState state e i
	     					      e -> updState state e 1

-- DEFUSE EXAMPLE

data DefUse x = Def x (Exp x) | Use x
	 
trace :: Eq x => [DefUse x] -> Store x -> ([(x,Int)],Store x)
trace (Def x e:s) store = trace s $ update store x $ exp2store e store
trace (Use x:s) store   = ((x,store x):s',store')
			  where (s',store') = trace s store
trace _ store	        = ([],store)

def :: Eq x => x -> Exp x -> State (Store x) ()
def x e = State $ \store -> ((),update store x $ exp2store e store) 

use :: x -> State (Store x) Int
use x = State $ \store -> (store x,store) 
	       
traceT :: Eq x => [DefUse x] -> State (Store x) [(x,Int)]  
traceT (Def x e:s) = do def x e; traceT s
traceT (Use x:s)   = do i <- use x; s' <- traceT s
		        return $ (x,i):s'
traceT _           = return []
	       
traceIO :: (Eq x,Show x) => [DefUse x] -> StateT (Store x) IO ()  
traceIO (Def x e:s) = do lift' $ def x e
			 traceIO s
traceIO (Use x:s)   = do i <- lift' $ use x
		         lift $ putStrLn $ show (x,i)
		         traceIO s
traceIO _           = return ()
     
data V = X | Y | Z deriving (Eq,Show)

dflist :: [DefUse V]
dflist = [Def X $ Con 1,Use X,Def Y $ Con 2,Use Y,Def X $ Sum [Var X,Var Y],
	  Use X,Use Y]
		  
tra,traT :: [(V,Int)]
tra  = fst $ trace dflist $ const 0	 	-- > [(X,1),(Y,2),(X,3),(Y,2)]    
traT = fst $ runS (traceT dflist) $ const 0 	-- > [(X,1),(Y,2),(X,3),(Y,2)] 

traIO :: IO ((),Store V)
traIO = runST (traceIO dflist) $ const 0 	-- > (X,1)
						--   (Y,2)
						--   (X,3)
						--   (Y,2)
                           
-- GENERIC EXPRESSION COMPILER

-- basic compilers

some, many :: MonadPlus m => m a -> m [a]
some p = do a <- p; as <- many p; return $ a:as
many p = msum [some p, return []]

type Compiler = StateT String Maybe

parse :: Compiler a -> String -> Maybe a
parse p str = do (a,_) <- runST p str; Just a

sat :: (Char -> Bool) -> Compiler Char
sat f = StateT $ \str -> do c:str <- return str
                            if f c then return (c,str) else mzero

char :: Char -> Compiler Char
char chr = sat (== chr)

nchar :: String -> Compiler Char
nchar chrs = sat (`notElem` chrs)

digit,letter,delim :: Compiler Char
digit  = msum $ map char ['0'..'9']
letter = msum $ map char $ ['a'..'z']++['A'..'Z']
delim  = msum $ map char " \n\t"

string :: String -> Compiler String
string = mapM char

bool :: Compiler Bool
bool = msum [do string "True"; return True,
             do string "False"; return False]

nat,int :: Compiler Int
nat = do ds <- some digit; return $ read ds
int = msum [nat, do char '-'; n <- nat; return $ -n]

identifier :: Compiler String
identifier = liftM2 (:) letter $ many $ nchar "(){};=!>+-*/^ \n\t"

token :: Compiler a -> Compiler a
token comp = do many delim; a <- comp; many delim; return a

tchar       = token . char
tstring     = token . string
tbool       = token bool
tint        = token int
tidentifier = token identifier

-- expression compiler

expC :: ExpAlg String exp -> Compiler exp
expC alg = do e <- summand; moreSummands e where
     summand = do e <- msum [scalar,factor]; moreFactors e
     factor  = msum [do i <- tint; power $ con alg i,
                     do x <- tidentifier; power $ var alg x,
	             do tchar '('; e <- expC alg; tchar ')'; power e]
     moreSummands e = msum [do tchar '-'; e' <- summand
			       moreSummands $ sub alg e e',
			    do es <- some $ do tchar '+'; summand
			       moreSummands $ sum_ alg $ e:es,
			    return e]
     moreFactors e  = msum [do tchar '/'; e' <- factor
			       moreFactors $ div_ alg e e',
			    do es <- some $ do tchar '*'; msum [scalar,factor]
		               moreFactors $ prod alg $ e:es,
			    return e]
     power e  = msum [do tchar '^'; i <- tint; return $ expo alg e i,
	       	      return e]
     scalar = do i <- tint
                 msum [do tchar '*'
                          msum [do e <- scalar; return $ scal alg i e,
                                do x <- tidentifier; e <- power $ var alg x
                                   return $ scal alg i e,
                                do tchar '('; e <- expC alg; tchar ')'
                                   return $ scal alg i e],
                       power $ con alg i]

-- full compiler  

compile :: String -> Int -> IO ()
compile file n = readFileAndDo file h where
   h str = case n of 0 -> act (expC termAlg) $ mkTree out . show
           	     1 -> act (expC termAlg) $ mkTree out . show . reduceE
           	     2 -> act (expC termAlg) $ mkTree out . show . flip diff "x"
           	     3 -> act (expC storeAlg) loopR
           	     4 -> act (fmap exp2store $ expC termAlg) loopR
           	     5 -> act (fmap exp2storeR $ expC termAlg) loopR
           	     6 -> act (expC codeAlg) loopC
           	     7 -> act (fmap exp2code $ expC termAlg) loopC
           	     8 -> act (fmap (evalTree . exp2tree) $ expC termAlg) loopR
           	     9 -> act (fmap evalTree $ expC treeAlg) loopR
           	     _ -> act (fmap exp2text $ expC termAlg) $ loopT out
           where out = file ++ "Result"
           	 act :: Compiler a -> (a -> IO ()) -> IO ()
           	 act comp continue = case runST comp str of 
           	                     Just (a,"") -> continue a
				     Just (a,str) 
				       -> do putStrLn $ "unparsed suffix: "++str
			                     continue a
		                     _ -> putStrLn "syntax error"

loopR :: (Store String -> Int) -> IO ()
loopR val = do (store,b) <- input
               when b $ do putStrLn $ "result = "++show (val store); loopR val
	   
loopT :: String -> (Store String -> (String,Int)) -> IO ()
loopT file val = do (store,b) <- input
                    when b $ do writeFile file $ fst $ val store; loopT file val
	            	   
loopC :: [StackCom String] -> IO ()
loopC code = do writeFile "code" $ fold2 f "" [0..] code; loop where
	     f str n c = str ++ '\n':replicate (5-length lab) ' ' ++ 
	   		 lab ++ ": "++ show c where lab = show n
	     loop = do (store,b) <- input
		       let (result:_,_) = execute code ([],store)
		       when b $ do putStrLn $ "result = "++show result; loop 
			   
input :: IO (Store String,Bool)
input = do putStrLn "Enter variables!"; str <- getLine
           let vars = words str
	   putStrLn "Enter values!"; str <- getLine
	   let vals = map read $ words str
	       vals' = vals++replicate (length vars-length vals) 0
	   return (fold2 update (const 0) vars vals', not $ null vars)

-- Examples

x = Var "x"
y = Var "y"
z = Var "z"

t1  = Sum [5:*Con 11,6:*Con 12,Prod[x,y,z]]        	 -- 5*11+6*12+x*y*z
t2  = Sum [Prod [x,y,z], Con 127]			 -- x*y*z+127
t3  = Sum [11 :* (x :^ 3),5 :* (x :^ 2),16 :* x,Con 33]  -- 11*x^3+5*x^2+16*x+33
t4  = Sum [11 :* (x :^ 3),5 :* (x :^ 2),16 :* x,Con 33,22 :* (x :^ 2)] 
t5  = Sum [5 :* (x :^ 2),22 :* (x :^ 2)] 
t6  = Prod [x :^5,Prod [x :^5,x :^6]]
t7  = Sum [11:*(x:^3),5:*(x:^2),16:*x,Con 33,x:-(y:-z),
           Prod[x:^5,Sum[x:^5,x:^6]]]
t8  = Sum [11:*(x:^3),5:*(x:^2),16:*x,Con 33,Sum[x,y,z]:-z,
           Prod[x:^5,Sum[x:^5,x:^6]]]
t9  = Prod [x,Con 5,5:*Prod[x:-y,y,z]]
t10 = Sum [Var"x":^4,5:*(Var"x":^3),11:*(Var"x":^2),Con 222]
t11 = t10 >>= \"x" -> 2:*Con 2 
t12 = Con 5:/(Con 4:-Var"x")

store1 = \case "x" -> 4; "y" -> 55

val1 = exp2store t10 store1          		-- > 974
val2 = exp2store t11 $ const 0			-- > 974
val3 = exp2storeM t10 store1			-- > Just 974
val4 = exp2storeM t12 store1			-- > Nothing
val5 = exp2storeR t10 store1			-- > 974
val6 = runMT (exp2storeRT t10) store1		-- > Just 974
val7 = runMT (exp2storeRT t12) store1		-- > Nothing

-- Inhalt von "store1": [("x",4),("y",55)]
-- Inhalt von "store2": [("z",4),("y",55)]

val8 = runMT $ exp2storeIO "store1" t10 	-- > "x" = 4
						--   "x" = 4
						--   "x" = 4
						--   Just 974
val9 = runMT $ exp2storeIO "store1" t12		-- > "x" = 4
						--   divide by zero
						--   Nothing
val10 = runMT $ exp2storeIO "store2" t10	-- > "x" is undefined
						--   Nothing
val11 = runMT $ exp2storeIO "store2" t12	-- > "x" is undefined
						--   Nothing
val12 = runMT $ exp2storeIO "store3" t10	-- > store3 does not exist
						--   "x" is undefined
						--   Nothing

exp1 = "5*11+6*12+x*y*z"
exp2 = "11*x^3+5*x^2+16*x+33"
exp3 = "x^3*x^5"
exp4 = "5*(6*(11*(x+y+z)*14+(c+g+b)*22)+44*(gg+hh))"
exp5 = "x-x-x"
exp6 = "11*x^3*x^4+33+3*x+5*x^2+16*x+(6*x^2+55)*5*x^2"
exp7 = "x^4+5*x^3+11*x^2+222"
exp8 = "exp2store(Sum (Var(x):^4, 5:*(Var(x):^3), 11:*(Var(x):^2), Con 222))"
exp9 = "5*6*7+x-5*2*3"
exp10 = "9*x^2+xx/x-x*5*z+yy+5*6*x+yy/5*6/5"

reduce1 = reduce "exp8" [evalStep store1] 

{- compile "exp1" 0		  --> t1	
   compile "exp1" 1		  --> t3 -showExp-> 127+x*y*z
   showExp $ diff t3 "x"  	  --> 33*x^2+10*x+16
   compile "exp2" 2  		  --> 33*x^2+10*x+16
   compile "exp7" 2  		  --> 4*x^3+15*x^2+22+x
   compile "exp2" 3..9 /\ x = 5   --> 1613
   compile "exp9" 10 /\ x = 66 	  --> The value of 6*7 is 42.
   				      The value of 5*6*7 is 210.
   				      The value of x is 66.
   				      The value of 5*6*7+x is 276.
   				      The value of 2*3 is 6.
   				      The value of 5*2*3 is 30.
   				      The value of 5*6*7+x-5*2*3 is 246. -}

