{-# LANGUAGE FlexibleInstances, LambdaCase #-}

module Painter where

----------------------------------------- Copyright (c) Peter Padawitz, May 2017

---- Manual: http://fldit-www.cs.uni-dortmund.de/~peter/Haskellprogs/Painter.pdf

import Prelude hiding (catch)
import Control.Exception
import Control.Applicative(Applicative(pure,(<*>)),Alternative(empty,(<|>)))
import Control.Monad
import Data.Complex
import Data.Maybe
import System.Directory
import Movie(Pos,movie,movies,movieSelect,html,(&),mkFile)

type Point  = (Float,Float)
type LPoint = (String,Float,Float)
type CPoint = (RGB,Float,Float)
type Path   = [Point]
type LPath  = [LPoint]
type CPath  = (RGB,Int,Path)
type CLPath = (RGB,LPath)

infixr 2 `union`
infixr 3 `meet`
 
update :: Eq a => (a -> b) -> a -> b -> a -> b
update f a b x = if x == a then b else f x

fold2 :: (a -> b -> c -> a) -> a -> [b] -> [c] -> a
fold2 f a (b:bs) (c:cs) = fold2 f (f a b c) bs cs
fold2 _ a _ _           = a

context s i = take i s++drop (i+1) s

updList s i a = take i s++a:drop (i+1) s

insert,remove :: Eq a => a -> [a] -> [a]
insert x s@(y:s') = if x == y then s else y:insert x s'
insert x _        = [x]
remove = filter . (/=)

union,diff,meet :: Eq a => [a] -> [a] -> [a]
union = foldl $ flip insert 
diff  = foldl $ flip remove
meet  = filter . flip elem

unionMap :: Eq b => (a -> [b]) -> [a] -> [b]
unionMap f = foldl union [] . map f

supset,subset,shares,disjoint :: Eq a => [a] -> [a] -> Bool
supset s   = all (`elem` s)  
subset     = flip supset
shares s   = any (`elem` s)
disjoint s = not . shares s

(***) :: (a -> b) -> (a -> c) -> a -> (b,c)
(***) = liftM2 (,)

(&&&),(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) = liftM2 (&&)
(|||) = liftM2 (||)
	        		  
liftR :: (Eq a,Eq b) => [(a,b)] -> [a] -> [b] -> Bool
liftR rel as bs = all (\a -> any (\b -> (a,b) `elem` rel) bs) as &&
		  all (\b -> any (\a -> (a,b) `elem` rel) as) bs

sort (x:s) = sort [y | y <- s, x <= y]++x:sort [y | y <- s, x > y]
sort s     = s

indices_ s = [0..length s-1]
 
zipWith4 f (a:as) (b:bs) (c:cs) (d:ds) = f a b c d:zipWith4 f as bs cs ds
zipWith4 _ _ _ _ _ 		       = []

newlines (str:strs@(_:_)) = str++",\n"++newlines strs
newlines strs             = concat strs

searchGet :: (a -> Bool) -> [a] -> Maybe (a,Int)
searchGet f = g 0 where g i (a:s) = if f a then Just (a,i) else g (i+1) s
                        g _ _     = Nothing
 
add1 :: Num a => (a,a) -> a -> (a,a)
add1 (x,y) a = (x+a,y)
 
add2,sub2 :: Num a => (a,a) -> (a,a) -> (a,a)
add2 (x,y) (a,b) = (a+x,b+y)
sub2 (x,y) (a,b) = (a-x,b-y)
 
apply2 f (x,y) = (f x,f y)

float :: RealFloat a => Int -> a
float = fromInteger . toInteger
	 
lgW a = float $ 5*length a

float2 (x,y) = (float x,float y)

round2 :: (RealFrac b, Integral d, RealFrac a, Integral c) => (a,b) -> (c,d)
round2 (x,y) = (round x,round y)

-- minmax ps computes minimal and maximal coordinates of the point list ps.

minmax ps@((x,y):_) = foldl f (x,y,x,y) ps
             where f (x1,y1,x2,y2) (x,y) = (min x x1,min y y1,max x x2,max y y2)
minmax _ = (0,0,0,0)

max2:: (Ord a,Num a) => [(a,a)] -> (a,a)
max2 = foldl1 f where f (x,y) (a,b) = (max x a,max y b)

search :: (a -> Bool) -> [a] -> Maybe Int
search f = g 0 where g i (x:s) = if f x then Just i else g (i+1) s
                     g _ _     = Nothing
                     
rel2fun :: Eq a => [(a,b)] -> a -> [b]
rel2fun = foldl f $ const [] where f g (a,b) = update g a $ b:g a
                     
fun2invrel :: (a -> [b]) -> [a] -> [(b,a)]
fun2invrel f as = [(b,a) | a <- as, b <- f a]
		     
fixpt :: (a -> a -> Bool) -> (a -> a) -> a -> a
fixpt rel step a = if rel b a then a else fixpt rel step b where b = step a

-- READ, WRITE AND DRAW

readFileContinue :: String -> a -> (String -> IO a) -> IO a
readFileContinue file a act = do str <- readFile file `catch` handler
		                 if null str 
		                    then do putStrLn $ file ++" does not exist"
		            		    return a
			         else act str
			      where handler :: IOError -> IO String
			      	    handler _ = return ""

readFileAndDo :: String -> (String -> IO ()) -> IO ()
readFileAndDo file = readFileContinue file ()
				    
test :: (Read a,Show b) => (a -> b) -> IO ()
test f = readFileAndDo "source" $ writeFile "target" . show . f . read
			      
type Svg a = Point -> a -> (String,Pos)
	
readAndDraw :: Float -> Float -> String -> Svg a -> (String -> Maybe a) -> IO ()
readAndDraw hor ver file draw parse = readFileAndDo file $ 
			         drawSingle hor ver file draw . fromJust . parse
			    	                      	
drawSingle :: Float -> Float -> String -> Svg a -> a -> IO ()
drawSingle hor ver file draw a = do b <- doesDirectoryExist "Pix"
			            when (not b) $ createDirectory "Pix"
			            scale a
        where scale a = if hor+ver == 0 then getScaleAndAct act $ scale a 
        			        else act (hor,ver) ""
                        where act hv back = writeFile ("Pix/" ++ file ++ ".svg")
			    	                      $ embed back $ draw hv a

mkHtml :: Bool -> String -> Int -> IO ()
mkHtml frame dir n = html frame ("Pix/" ++ dir) $ map (mkFile dir) [1..n]

-- For nonempty lists as, drawList file draw as creates the directory 
-- Pix/file, draws as!!i for all i in indices(ts) with a horizontal resp. 
-- vertical scaling factor 10 resp. 40 into Pix/file/i.svg and colors red resp. 
-- green all nodes of the subtree of ts!!i with root p if i is even resp. odd.

drawList :: String -> (Int -> Svg a) -> [a] -> IO ()
drawList file draw as = do b <- doesDirectoryExist "Pix"
			   when (not b) $ createDirectory "Pix"
			   mkHtml True file n
	       		   drawDir 10 40 file draw as n
			where n = length as

drawDir :: Float -> Float -> String -> (Int -> Svg a) -> [a] -> Int -> IO ()
drawDir hor ver dir draw as n = do b <- doesDirectoryExist pdir
                   	           when b $ removeDirectoryRecursive pdir
                   	           createDirectory pdir
                   	           scale
                where pdir = "Pix/" ++ dir
                      scale = if hor+ver == 0 then getScaleAndAct act scale 
                       			      else act (hor,ver) ""
                      act hv _ = mapM_ write [1..n]
                          where write i = writeFile ("Pix/" ++ mkFile dir i) 
              			              $ embed "" $ draw i hv $ as!!(i-1)

getScaleAndAct :: (Point -> String -> IO ()) -> IO () -> IO ()
getScaleAndAct act continue = do putStrLn prompt1
    		                 str <- getLine
    		                 let strs = words str
    		                 when (length strs >= 2) $ do
		                      let [hor,ver] = map read $ take 2 strs
			                  rest = drop 2 strs
			              act (hor,ver) $ if null rest then "" 
			          		      else head rest
			              continue

prompt1 = "Enter both a horizontal and a vertical scaling factor and - " ++
          "optionally - a background image (svg, jpg, png, gif or pdf)!"
			    			       
prompt2 = "Enter both a horizontal and a vertical scaling factor!"

-- drawText file loads a string from file and writes it in columns each of which
-- consists of at most 37 lines into file.svg.

drawText :: String -> IO ()
drawText file = do b <- doesDirectoryExist "Pix"
                   when (not b) $ createDirectory "Pix"
		   readFileAndDo file act 
	        where act str = writeFile ("Pix/" ++ file ++ ".svg")
	        			  $ embed "" (code,size)
		        where code = concatMap drawCol [0..nrCols-1]
			      size = (9*pos nrCols+45,20*maximum heights)
			      ls = lines str
			      nrLines = length ls
			      matrix = split ls
			      nrCols = length matrix
			      heights = map length matrix
			      split s = if length s <= 37 then [s]
			     	        else take 37 s:split (drop 37 s)
			      pos 0 = 1
			      pos i = pos k+maximum (map length $ matrix!!k)
		                      where k = i-1
		              drawCol i = concat $ zipWith (drawLine $ pos i) 
	 			                   [0..heights!!i-1] $ matrix!!i
			      drawLine p k = text black $ float2 (9*p,24+k*20)
	 
-- TREES

data Tree a = V a | F a [Tree a] deriving (Eq,Show)

class HasVars a where isVar :: a -> Bool; getVar :: a -> a

instance HasVars Int where isVar = (<0); getVar x = -x
instance HasVars String where isVar str = not (null str) && head str == 'V'
			      getVar (_:str) = str

readTree :: (Read a,HasVars a) => ReadS (Tree a)
readTree s = forks++vars++consts
	     where vars   = [(V $ getVar a,s1) | (a,s1) <- reads s, isVar a]
	           consts = [(F a [],s1) | (a,s1) <- reads s, not $ isVar a]
		   forks  = [(F a $ t:ts,s4) | (a,s1) <- reads s,
		                               ("(",s2) <- lex s1,
					       (t,s3) <- readTree s2,
                                               (ts,s4) <- trees s3]
		   trees s = [([],s1) | (")",s1) <- lex s] ++
		   	     [(t:ts,s3) | (",",s1) <- lex s,
		   		          (t,s2) <- readTree s1,
					  (ts,s3) <- trees s2]

instance (Read a,HasVars a) => Read (Tree a) where readsPrec _ = readTree

t42 = read "5(66,77(8,-5))" :: Tree Int
t43 = read "\"5\"(\"66\",\"77\"(\"8\",\"V3\"))" :: Tree String

isLeaf (V _)    = True
isLeaf (F _ []) = True
isLeaf _        = False

root :: Tree a -> a
root (V a)   = a
root (F a _) = a

subtrees :: Tree a -> [Tree a]
subtrees (F _ ts) = ts
subtrees _        = []

vars :: Eq a => Tree a -> [a]
vars (V a)    = [a]
vars (F _ ts) = unionMap vars ts

height :: Tree a -> Int
height (F _ ts) = foldl max 0 (map height ts)+1
height _        = 1

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (V a)    = V $ f a
mapTree f (F a ts) = F (f a) $ map (mapTree f) ts
  
(>>>) :: Tree a -> (a -> Tree a) -> Tree a
V a >>> sub    = sub a     
F a ts >>> sub = F a $ map (>>> sub) ts

type Node = [Int]

(<<),(<<=) :: Node -> Node -> Bool
(m:p) << (n:q) = m == n && p << q
_ << p         = not $ null p
p <<= q        = p << q || p == q

nodes,leaves :: Tree a -> [Node]
nodes (F _ ts)  = []:[i:p | (t,i) <- zip ts [0..], p <- nodes t]
nodes _         = [[]]
leaves (F _ []) = [[]]
leaves (F _ ts) = [i:p | (t,i) <- zip ts [0..], p <- leaves t]
leaves _        = [[]]

label :: Tree a -> Node -> a
label t []           = root t
label (F _ ts) (n:p) = label (ts!!n) p

-- zipWithSucs f p s applies a binary function after zipping the list of
-- direct successor positions of p with s.

zipWithSucs :: (Node -> a -> b) -> Node -> [a] -> [b]
zipWithSucs f p = zipWith f $ map (\i -> p++[i]) [0..]

-- PARSER

data Parser a = P {runP :: String -> Maybe (a,String)}

instance Monad Parser where
         return a = P $ \str -> Just (a,str)
         P h >>= f = P $ (>>= \(a,st) -> runP (f a) st) . h
	 fail _ = mzero

instance MonadPlus Parser where
         mzero = P $ const Nothing
         P g `mplus` P h = P $ liftM2 mplus g h

{- enforced constraints: Functor f => Applicative f
   			 Applicative f => Alternative f
   			 Applicative m => Monad m
   			 (Alternative m, Monad m) => MonadPlus m -}

instance Functor Parser where fmap f = (>>= return . f)
 	 
instance Applicative Parser where pure = return
         			  mf <*> m = mf >>= flip fmap m

instance Alternative Parser where empty = mzero
 	 			  (<|>) = mplus

parse :: Parser a -> String -> Maybe a
parse p str = do (a,_) <- runP p str; Just a

some,many,nelist,list :: Parser a -> Parser [a]
some p = do a <- p; as <- many p; return $ a:as
many p = some p `mplus` return []
nelist p = do a <- p; msum [do tchar ','; as <- nelist p; return $ a:as, 
			    return [a]]
list p   = do tchar '['; msum [do tchar ']'; return [],
	                       do as <- nelist p; tchar ']'; return as]

sat :: (Char -> Bool) -> Parser Char
sat f = P $ \str -> do c:str <- return str
                       if f c then return (c,str) else mzero

char,tchar :: Char -> Parser Char
char  = sat . (==)
nchar = sat . (/=)
tchar = token . char

digit,letter,delim :: Parser Char
digit  = msum $ map char ['0'..'9']
letter = msum $ map char $ ['a'..'z']++['A'..'Z']
delim  = msum $ map char " \n\t"

nat,int :: Parser Int
nat = do ds <- some digit; return $ read ds
int = nat `mplus` do tchar '-'; n <- nat; return $ -n

string,tstring :: String -> Parser String
string  = mapM char
tstring = token . string

bool :: Parser Bool
bool = msum [do string "true"; return True,
             do string "false"; return False]
             
relation :: Parser (Int -> Int -> Bool)
relation = msum [do string "=="; return (==),
                 do string "/="; return (/=),
                 do string "<="; return (<=),
                 do string ">="; return (>=),
                 do string "<";  return (<),
                 do string ">";  return (>)]

token :: Parser a -> Parser a
token p = do many delim; a <- p; many delim; return a

quoted :: Parser String
quoted = do char '"'; x <- many $ nchar '"'; char '"'; return x

termEqs :: Parser (Tree String,[Equations String])
termEqs = do t <- term; return $ case t of F ";;" (t:ts) -> (t,map eqs ts)
				           _ -> (t,[])
	  where eqs (F ";" ts)    = [(t,u) | F "=" [t,u] <- ts]
	        eqs (F "=" [t,u]) = [(t,u)]
		eqs _             = []
	      
termNodes :: Parser (Tree String,[Node])
termNodes = do t <- term; tchar ','; ps <- list $ list nat; return (t,ps)

infixes = words ";; ; >> >< = \\/ /\\ == /= <= < >= > $ : + - * / ^^ ^ ."

term = term' infixes

term' :: [String] -> Parser (Tree String)
term' (a:as) = do t <- term' as
		  ts <- many $ do tstring a; term' as
 	          return $ if null ts then t else F a $ t:ts
term' _      = msum [tree,
		     do i <- token int; return $ F (show i) [],
                     do t <- token var; curryrest t,
                     do t <- tuple; curryrest t,
		     do t <- termList; curryrest t,
		     do x <- token function; curryrest $ F x []]

var,tree,tuple :: Parser (Tree String)

var = do x <- msum $ map char "xyzFGH"
	 s <- many $ mplus letter digit
	 return $ V $ x:s
		 
tree = msum [do tchar 'V'; x <- token quoted; return $ V x,
             do tchar 'F'; x <- token quoted; F _ ts <- termList
                return $ F x ts]

tuple = do tchar '('; msum [do tchar ')'; return $ F "()" [],
 			    do ts <- abstraction
 			       tss <- many $ do tchar '|'; abstraction
 			       tchar ')'; return $ F "&#955;" (ts++concat tss),
 			    do op <- getOp; guard (op /= "-")
 			       t <- term; tchar ')'
 			       return $ F "rsec" [F op [],t],
 			    do t <- term; op <- getOp; tchar ')'
 			       return $ F "lsec" [t,F op []],
	      		    do ts <- nelist term; tchar ')'
			       return $ case ts of [t] -> t; _ -> F "()" ts]
        where getOp = msum $ map tstring infixes
        
abstraction :: Parser [Tree String]
abstraction = do t <- term; tstring "->"; u <- term; return [t,u]

termList :: Parser (Tree String)
termList = msum [do tchar '['; t <- term; tstring ".."; u <- term; tchar ']'
	            return $ F "range" [t,u],
	         do ts <- list term; return $ F "[]" ts]

function :: Parser String
function = quoted `mplus` some (sat (`notElem` " \n\t()[]{},;<>=+-*/\\^:$.|"))

curryrest t = msum [do t <- tuple; curryrest $ applyTo t,
		    do t <- termList; curryrest $ applyTo t,
		    do ts <- fields; F x [] <- return t; return $ F x ts,
		    do x <- token function; curryrest $ applyTo $ F x [],
		    do t <- term; curryrest $ applyTo t,
		    return t] where
	      applyTo u = case t of F x [] -> F x $ if root u == "()"
		    		                    then subtrees u else [u] 
		                    _ -> F "$" [t,u]
		                    
fields :: Parser [Tree String]            -- Field entries are preceded by "FF".
fields = do tchar '{'; do ts <- fs; tchar '}'; return ts
	 where fs = do field <- token function; tchar '='; t <- term
	               let val = F ("FF"++field) [t]
	               msum [do tchar ','; ts <- fs; return $ val:ts, 
	       	             return [val]]

-- REDUCER

type Equations a = [(Tree a,Tree a)]
type Rules a     = Tree a -> Maybe (Tree a)

reduce :: String -> [Rules String] -> IO ()
reduce file rules = readFileAndDo file f where
	            redfile = file++"Reduction"
                    f str = case runP termEqs str of 
	  	            Just ((t,eqs),_) 
	  	              -> drawList redfile svg $ reduction t $ Just:
			      			        map applyEq eqs++rules
		            _ -> putStrLn "reduce error"
	            svg i = svgTreeSub $ if even i then red else green

-- reduction t rules subsequently applies to t the rules of rules!!0, 
-- then of rules!!1, etc., and stores the root positions of all redices and 
-- reducts.

reduction :: Tree String -> [Rules String] -> [(Tree String,Node)]
reduction t = foldl f [(t,[])] where
              f s rules = init s ++ iteration rules t [] where (t,_) = last s

-- iteration t rules applies rules to t after t has been simplified by applying
-- eval.

iteration :: Rules String -> Tree String -> Node -> [(Tree String,Node)]
iteration rules t p = case redStep eval t [] of 
 			   Just (u,q) | t /= u -> f u q
			   _ -> case redStep rules t [] of 
			   	     Just (u,q) | t /= u -> f u q
			             _ -> [(t,p)]
		      where f u q = (t,p):(t,q):iteration rules u q

-- redStep rules t p searches for the first leftmost-outermost redex in the 
-- subtree of t at position p and performs the corresponding reduction step.

redStep :: Rules String -> Tree String -> Node -> Maybe (Tree String,Node)
redStep rules t p = msum [do t <- rules t
			     Just (t,p),
			  do F a ts <- Just t
			     (ts,p) <- f ts p 0
			     Just (F a ts,p)] where
	            f ts p i = do t:ts <- Just ts
		                  case redStep rules t $ p++[i] of 
				       Just (t,p) -> Just (t:ts,p)
		 		       _ -> do (ts,p) <- f ts p $ i+1
					       Just (t:ts,p)

applyEq :: Equations String -> Rules String
applyEq eqs t = msum $ map f eqs where f (left,right) = do sub <- match t left
		   			  		   Just $ right >>> sub

match :: Tree String -> Tree String -> Maybe (String -> Tree String)
match t (V a) = Just $ update V a t
match (F a ts) (F b us) 
               | isJust n && b == "suc" = do n <- n
            			             g [F (show $ n-1) []] us
               | a == b && length ts == length us = g ts us
               | a == "[]" && b == ":" || 
                 a `elem` infixes && a == b = do t:ts <- Just ts
      					         [_,_] <- Just us
      					         g [t,F a ts] us 
	         where n = parse nat a
	               g (t:ts) (u:us) = do sub1 <- match t u
	    		                    sub2 <- g ts us
	    		                    guard $ map sub1 zs == map sub2 zs
	    		                    Just $ \x -> if x `elem` xs 
				         	         then sub1 x else sub2 x 
		                         where xs = vars u
		                               zs = meet xs $ unionMap vars us
                       g _ _ = Just V
match _ _ = Nothing

parseBool = parse bool . root

mkBool :: Bool -> Maybe (Tree String) 
mkBool True = Just $ F "true" [] 
mkBool _    = Just $ F "false" []

parseInt = parse int . root

mkInt :: Int -> Tree String
mkInt i = F (show i) []

jmkInt :: Int -> Maybe (Tree String)
jmkInt = Just . mkInt

mkList :: [Tree String] -> Maybe (Tree String)
mkList = Just . F "[]" 

getField :: String -> [Tree String] -> Maybe (Tree String)
getField d (t:ts) = case t of F ('F':'F':d') [t] | d == d' -> Just t
			      _ -> getField d ts
getField d _      = Nothing	
		
eval,flatten :: Rules String

eval (F "\\/" ts)   | all isJust bs       = mkBool $ or $ map fromJust bs
                                            where bs = map parseBool ts
eval (F "/\\" ts)   | all isJust bs    	  = mkBool $ and $ map fromJust bs
                                            where bs = map parseBool ts
eval (F "not" [t])  | isJust b		  = mkBool $ not $ fromJust b
					    where b = parseBool t
eval (F "==" [t,u]) | t == u              = mkBool True
eval (F op [t,u])   | isJust rel && isJust i && isJust k
				          = mkBool $ fromJust rel (fromJust i)
				     		   		  (fromJust k) 
				            where rel = parse relation op
				                  i = parseInt t; k = parseInt u
eval (F "suc" [t]) | isJust i             = jmkInt $ fromJust i+1 
					    where i = parseInt t
eval (F "-" [t,u]) | isJust i && isJust k = jmkInt $ fromJust i-fromJust k
			                    where i = parseInt t; k = parseInt u
eval (F "^" [t,u]) | isJust i && isJust k = jmkInt $ fromJust i^fromJust k
			                    where i = parseInt t; k = parseInt u
eval (F "+" ts@(_:_:_)) | all isJust is   = jmkInt $ sum $ map fromJust is
				            where is = map parseInt ts
eval (F "*" ts@(_:_:_)) | all isJust is   = jmkInt $ product $ map fromJust is
				            where is = map parseInt ts
eval (F ":" [t,F "[]" ts])                = mkList $ t:ts
eval (F "id" [t])                         = Just t
eval (F "ite" [t,u,v]) | isJust b         = Just $ if fromJust b then u else v
					    where b = parseBool t
eval (F "range" [t,u]) | isJust i && isJust k 
			                  = mkList $ map f [fromJust i..
			                  		    fromJust k]
			                    where i = parseInt t; k = parseInt u
			                    	  f i = F (show i) [] 
eval (F "$" [F "&#955;" (p:t:ts),u])      = msum [do sub <- match u p
						     Just $ t >>> sub,
				                 Just $ F "$" [F "&#955;" ts,u]]
eval (F "$" [F "." [t,u],v])              = Just $ F "$" [t,F "$" [u,v]]
eval (F "$" [F "lsec" [t,F op []],u])  	  = Just $ F op [t,u]
eval (F "$" [F "rsec" [F op [],t],u])     = Just $ F op [u,t]
eval (F d [F _ ts@(F ('F':'F':_) [_]:_)]) = getField d ts
eval (F "$" [F f [],t]) 		  = Just $ F f [t]
eval _					  = Nothing
	       
flatten t = do F op ts <- Just t
               guard $ op `elem` words "\\/ /\\ + *" 
               let u = norm op ts
               guard $ u /= t
	       Just u
	       
impl t = msum [do F "\\/" ts <- Just t
                  let (prem,conc,b) = split ts
                  if b then mkBool True 
		       else Just $ F "==>" [norm "/\\" prem,norm "or" conc],
	       do F "/\\" ts <- Just t
	          let u = norm "/\\" ts
	          guard $ t /= u
		  Just u]
         where split (t:ts) = if root t == "not" 
		              then if u `elem` vs then empty else (u:us,vs,b) 
			      else if t `elem` us then empty else (us,t:vs,b) 
		              where empty = ([],[],True)
		          	    (us,vs,b) = split ts
		          	    u = head $ subtrees t
               split _      = ([],[],False)
	       
norm :: String -> [Tree String] -> Tree String
norm op []  = zero op
norm op [t] = t
norm op ts  = case args (F op ts) of [] -> zero op; [t] -> t; ts -> F op ts
         where args (F x ts) | x == op = unionMap args $ filter (/= zero op) ts
               args t	               = if t == zero op then [] else [t]

zero "*"   = F "true" []
zero "/\\" = F "true" []
zero _     = F "false" []

distribute :: String -> String -> Rules String
distribute a b (F c ts) | a == c && isJust ti 
		 = Just $ F b $ map (F a . updList ts i) us 
		   where ti = searchGet ((== b) . root) ts
		         (F _ us,i) = fromJust ti
distribute _ _ _ = Nothing

-- Examples

lisa     = reduce "lisa" [] 
fact     = reduce "fact" [] 
factI    = reduce "factI" [] 
fiter    = reduce "fiter" [] 
lamb1    = reduce "lamb1" [] 
lamb2    = reduce "lamb2" [] 
lamb3    = reduce "lamb3" []
lamb4    = reduce "lamb4" []
eval1    = reduce "eval1" []
eval2    = reduce "eval2" [flatten] 
dist1    = reduce "dist1" [distribute "*" "+",flatten] 		   
dist2    = reduce "dist2" [flatten] 				   
gentzen  = reduce "gentzen" [distribute "\\/" "/\\",flatten,impl] 

-- TREE LOGICS (XCTL = XPath and CTL)

data XCTL lab nodeSet nodeRel = 
     XCTL {par,seq_ :: nodeRel -> nodeRel -> nodeRel,
     	   closure,inv :: nodeRel -> nodeRel,
     	   restrict :: nodeRel -> nodeSet -> nodeRel,
     	   self,child,parent,next,prev,descendant,ancestor,folSib,preSib,
     	   following,preceding,equal,equiv :: nodeRel,
     	   atom :: (lab -> Bool) -> nodeSet,
     	   true_,false_ :: nodeSet,
     	   and_,or_ :: nodeSet -> nodeSet -> nodeSet,
     	   exists,forall :: nodeRel -> nodeSet -> nodeSet,
     	   not_,ex,ax,ef,af,eg,ag :: nodeSet -> nodeSet}

xalg :: Eq lab => Tree lab -> XCTL lab [Node] (Node -> [Node])
xalg t = XCTL {par = (#), 
               seq_ = (>=>),
               closure = closure, 
               inv = \r -> rel2fun $ fun2invrel r $ nodes t,
               restrict = \r -> \s -> \w -> r w `meet` s,
               self = self,
               child = child,
               parent = parent,
               next = next,
               prev = prev,
               descendant = descendant,
               ancestor = ancestor,			
               folSib = folSib,
               preSib = preSib,	
               following = (self # ancestor) >=> folSib >=> (self # descendant),
               preceding = (self # ancestor) >=> preSib >=> (self # descendant),
               equal = rel2fun $ fixpt supset stepEqual nodePairs,
               equiv = rel2fun $ fixpt supset stepEquiv nodePairs,
               atom = \pred -> filter (pred . label t) $ nodes t,
               true_ = nodes t,
               false_ = [],
               and_ = meet,
               or_ = union,
               exists = exists,
               forall = forall,
               not_ = diff $ nodes t,
               ex = ex,
               ax = ax,
               ef = exists (self # descendant),     
               af = \s -> fixpt subset (stepAF s) [],
               eg = \s -> fixpt supset (stepEG s) $ nodes t,
               ag = forall (self # descendant)}
         where (#) = liftM2 union
               closure r = fixpt le ((r#) . (r>=>)) r where
                           le r r' = all (liftM2 subset r r') $ nodes t 
               self = return
               child v = [w | w <- nodes t, not $ null w, init w == v]
               parent [] = []
               parent w  = [init w]
               next [] = [] 
               next w  = if v `elem` nodes t then [v] else []
               		 where v = init w++[last w+1]
               prev [] = [] 
               prev w  = if v `elem` nodes t then [v] else []
               		 where v = init w++[last w-1]
               descendant = closure child
               ancestor = closure parent		
               folSib = closure next
               preSib = closure prev
               nodePairs = [(v,w) | v <- s, w <- s, v < w] where s = nodes t
               stepEqual rel = filter f nodePairs where
               	               f (v,w) = label t v == label t w &&
	  		                 length (child v) == length (child w) &&
	                                 zip (child v) (child w) `subset` rel
               stepEquiv rel = filter f nodePairs where
               	               f (v,w) = label t v == label t w &&
	  		                 liftR rel (child v) (child w)
	       exists r s = [w | w <- nodes t, r w `shares` s]
	       forall r s = [w | w <- nodes t, r w `subset` s]
	       ex = exists child
	       ax = forall child
	       stepAF s s' = s `union` ax s' `meet` (ex $ nodes t)
	       stepEG s s' = s `meet` (ex s' `union` ax [])
	       			       
foldSet :: XCTL String nodeSet nodeRel -> Tree String -> Maybe nodeSet
foldSet alg = f where f (F "true" [])   = Just $ true_ alg
                      f (F "false" [])  = Just $ false_ alg   
                      f (F a [])        = Just $ atom alg (== a)   
                      f (F "rsec" [F op [],F a []]) | isJust i && isJust rel      
                      			= Just $ atom alg p where
                     			  i = parse int a                     			  
                     			  rel = parse relation op
                     			  p b = isJust k && fromJust rel 
                     			  	       (fromJust k) (fromJust i)
                       			        where k = parse int b
                      f (F "/\\" ts)    = do guard $ not $ null ts
                       			     rs <- mapM f ts
                      			     Just $ foldl1 (and_ alg) rs
                      f (F "\\/" ts)    = do guard $ not $ null ts
                       			     rs <- mapM f ts
                      			     Just $ foldl1 (or_ alg) rs
                      f (F "&exist;" [t,u])  = do r <- foldRel alg t; s <- f u
                      			          Just $ exists alg r s
                      f (F "&forall;" [t,u]) = do r <- foldRel alg t; s <- f u
                      			          Just $ forall alg r s
                      f (F "not" [t])   = do s <- f t; Just $ not_ alg s
                      f (F "EX" [t])    = do s <- f t; Just $ ex alg s
                      f (F "AX" [t])    = do s <- f t; Just $ ax alg s
                      f (F "EF" [t])    = do s <- f t; Just $ ef alg s
                      f (F "AF" [t])    = do s <- f t; Just $ af alg s
                      f (F "EG" [t])    = do s <- f t; Just $ eg alg s
                      f (F "AG" [t])    = do s <- f t; Just $ ag alg s
                      f _ 		= Nothing

foldRel :: XCTL String nodeSet nodeRel -> Tree String -> Maybe nodeRel
foldRel alg = f where f (F "+" ts)     = do guard $ not $ null ts
                       			    rs <- mapM f ts
                      			    Just $ foldl1 (par alg) rs
                      f (F "/" ts)     = do guard $ not $ null ts
                       			    rs <- mapM f ts
                      			    Just $ foldl1 (seq_ alg) rs
                      f (F "clos" [t]) = do r <- f t; Just $ closure alg r
                      f (F "inv" [t])  = do r <- f t; Just $ inv alg r
                      f (F ">>" [t,u]) = do r <- f t; s <- foldSet alg u
                      			    Just $ restrict alg r s
                      f (F x []) = case x of 
                                        "self" -> Just $ self alg
                                        "child" -> Just $ child alg
                                        "parent" -> Just $ parent alg
                                        "next" -> Just $ next alg
                                        "prev" -> Just $ prev alg
                                        "descendant" -> Just $ descendant alg
                                        "ancestor" -> Just $ ancestor alg
                                        "folSib" -> Just $ folSib alg
                                        "preSib" -> Just $ preSib alg
                                        "following" -> Just $ following alg
                                        "preceding" -> Just $ preceding alg
                                        "equal" -> Just $ equal alg
                                        "equiv" -> Just $ equiv alg
                                        _ -> Nothing
                      f _ = Nothing
                     
-- Examples 
       
rel1 = "(child >> (a /\\ (descendant >< d))) / child / (descendant >> c)"

showRel = mkTree "rel1" rel1  

testrel1 = drawNodeRel "baum9" rel1 	 	
testrel2 = drawNodeRel "baum9" "preceding"   	 	
testrel3 = drawNodeRel "baum9" "following" 	 	
testrel4 = drawNodeRel "baum9" "equal" 
testrel5 = drawNodeRel "baum9" "equiv" 	 	

testset1 = drawNodeSet "baum10" "EX(true)\\/d" 
testset2 = drawNodeSet "baum10" "EF(22)"      
testset3 = drawNodeSet "baum10" "AF(22)" 
testset4 = drawNodeSet "baum10" "EG((<=22)\\/66)" 
testset5 = drawNodeSet "baum10" "AG((<=22)\\/66)" 

-- TREE PAINTER
	
drawTree :: String -> IO ()
drawTree file = readAndDraw 10 40 file svgTree $ Just . read
				      
-- drawTerm{C} file reads the contents of file, compiles it into a tree t of
-- type Tree String and draws t with a horizontal resp. vertical scaling factor
-- 10 resp. 40 into file.svg.
-- drawTerm{C}I asks for the scaling factors and an optional background before
-- drawing t.
-- drawTermC colors the nodes at level i with hue 1 red (height t) i.
-- drawTermNodes also expects a list of nodes that will be colored green.

drawTerm file      = readAndDraw 10 40 file svgTree $ parse term
drawTermI file     = readAndDraw 0 0 file svgTree $ parse term
drawTermC file     = readAndDraw 10 40 file svgTreeC $ parse term
drawTermCI file    = readAndDraw 0 0 file svgTreeC $ parse term
drawTermNodes file = readAndDraw 10 40 file svgTreeSet $ parse termNodes
			   
mkTree file str  = do writeFile file str; drawTerm file
mkTreeC file str = do writeFile file str; drawTermC file
			    	                    
-- drawTerms{C} file reads a nonempty list ts of trees from file, creates the 
-- directory Pix/file and draws ts!!i for all i in indices(ts) with a horizontal
-- resp. vertical scaling factor 10 resp. 40 into Pix/file/i.svg.

drawTerms file  = readFileAndDo file $ drawList file (const svgTree) .
				       fromJust . parse (nelist term)
drawTermsC file = readFileAndDo file $ drawList file (const svgTreeC) .
				       fromJust . parse (nelist term)

-- Given an XCTL formula form of type nodeSet (see above), drawNodeSet file form 
-- reads a tree t from file, draws t with a horizontal resp. vertical scaling 
-- factor 10 resp. 40 into file_set.svg and colors green all nodes of form(t).

drawNodeSet :: String -> String -> IO ()
drawNodeSet file form = readFileAndDo file act where
	           act tstr = case phi of Just set -> draw svgTreeSet (t,set)
	         	                  _ -> draw svgTree $ F "no|formula" []
	         	      where t = fromJust $ parse term tstr
	         	            phi = (parse term >=> foldSet (xalg t)) form
	         	            draw = drawSingle 10 40 $ file++"_set"
	         	           	    
-- Given an XCTL formula form of type nodeRel (see above), drawNodeRel file form
-- reads a tree t from file, creates the directory Pix/file_rel, draws t with a 
-- horizontal resp. vertical scaling factor 10 resp. 40 for 
-- ws = [w in nodes(t) | form(t)(w)/=[]] and all i in indices(ws) into 
-- Pix/file_rel/i.svg  and colors ws!!i red and green all nodes of
-- form(t)(ws!!i).

drawNodeRel :: String -> String -> IO ()
drawNodeRel file form = readFileAndDo file act where
	 act tstr = case phi of Just rel -> drawL $ trips rel
	         	        _ ->  draw $ F "no|formula" []
	            where t = fromJust $ parse term tstr
	                  phi = (parse term >=> foldRel (xalg t)) form
	                  trips rel = [(t,w,ws) | w <- nodes t, 
	                      	                  let ws = rel w, not $ null ws]
	             	  draw = drawSingle 10 40 (file++"_set") svgTree
	             	  drawL = drawList (file++"_rel") $ const svgTreeRel
		                		
-- TREES WITH NODE POSITIONS

type PTree = Tree (String,Point)

nodeSize :: String -> Point
nodeSize a = (w,h) where (_,_,w,h) = textblock p0 a
	  
nodeWidth :: String -> Float
nodeWidth = fst . nodeSize	

-- mkPTree p (hor,ver) t adds positions in the plane to the nodes of t such that
-- p is the leftmost-uppermost corner of the smallest rectangle enclosing t.
-- hor and ver are the horizontal resp. vertical spaces between adjacent nodes. 
-- mkPTree True admits multiline node labels and interprets '%' as a line break
-- symbol.

mkPTree :: Point -> Point -> Tree String -> PTree
mkPTree (hor,ver) p = alignLeaves hor . f p
 where f (x,y) (V a)    = V (a,(x+nodeWidth a,y))
       f (x,y) (F a []) = F (a,(x+nodeWidth a,y)) []
       f (x,y) (F a ts) = if diff <= 0 then qt else translate diff qt
                         where diff = nodeWidth a-fst (treeSize pt)
			       hdiff = height a+maximum (map (height . root) ts)
			       height = (2*) . snd . nodeSize
			       pt:pts = map (f (x,y+ver+hdiff/2)) ts
			       qts = transTrees hor pt pts
			       qt = F (a,((g (head qts)+g (last qts))/2,y)) qts
			       g = fst . snd . root
			       n = length ts `div` 2

-- translate d t moves t by d units to the right.

translate :: Float -> PTree -> PTree
translate d = mapTree f where f (a,(x,y)) = (a,(x+d,y))

-- transTrees hor pt pts orders the trees of pt:pts with a horizontal space of 
-- hor units between adjacent trees. transTrees takes into account different 
-- heights of adjacent trees by shifting them to the left or to the right such 
-- that nodes on low levels of a tree may occur below a neighbour with fewer 
-- levels.

transTrees :: Float -> PTree -> [PTree] -> [PTree]
transTrees hor pt = f [pt]
         where f us (t:ts) = if d < 0 then f (map (translate $ -d) us++[t]) ts
                             else f (us++[translate d t]) $ map (translate d) ts
                          -- f (us++[if d < 0 then t else translate d t]) ts 
                             where d = maximum (map h us)+hor 
	      	                   h u = f (+) maximum u-f (-) minimum t
			                 where f = g $ min (height t) $ height u
               f us _      = us
               g _ op _ (V (a,(x,_)))    = h op x a
               g 1 op m (F (a,(x,_)) _)  = h op x a
               g n op m (F (a,(x,_)) ts) = m $ (h op x a):map (g (n-1) op m) ts
	       h op x = op x . nodeWidth

textblock :: Point -> String -> (Path,[String],Float,Float)
textblock (x,y) a = (map f [0..upb],ls,maximum $ map lgW ls,h)
                    where ls = lines a [] ""
		    	  upb = length ls-1
		    	  lines (x:'%':[]) ls b = ls++[b++[x,'%']]
		    	  lines (x:'%':a) ls b  = lines a (ls++[b++[x]]) ""
		    	  lines (x:a) ls b      = lines a ls $ b++[x]
			  lines _ ls b          = ls++[b]
			  f i = (x,y-h+20*float i)
			  h = 10*float upb
			  
treeLeft :: PTree -> Float
treeLeft = minimum . bounds
           where bounds t = f (root t) $ map bounds $ subtrees t
                 f :: (String,Point) -> [[Float]] -> [Float]
	         f (a,(x,y)) fss = x+nodeWidth a:concat fss
			  
treeSize :: PTree -> Point
treeSize = max2 . bounds
           where bounds t = f (root t) $ map bounds $ subtrees t
                 f :: (String,Point) -> [Path] -> Path
		 f (a,p) pss = add2 p (nodeSize a):concat pss

-- alignLeaves hor t replaces the leaves of t such that all horizontal gaps 
-- between neighbours become equal.

alignLeaves :: Float -> PTree -> PTree
alignLeaves hor (F a ts) = F a $ equalGaps hor $ map (alignLeaves hor) ts 
alignLeaves _ t          = t

equalGaps :: Float -> [PTree] -> [PTree]
equalGaps hor ts = if length ts > 2 then us++vs else ts
                   where (us,vs) = foldl f ([],[head ts]) $ tail ts
                         f (us,vs) v = if isLeaf v then (us,vs++[v])
	         	               else (us++transLeaves hor vs v,[v])
	
transLeaves :: Float -> [PTree] -> PTree -> [PTree]
transLeaves hor ts t = loop hor
             where loop hor = if x1+w1+hor >= x2-w2 then us else loop $ hor+1 
		        where us = transTrees hor (head ts) $ tail ts
		              [x1,x2] = map (fst . snd . root) [last us,t]
		              [w1,w2] = map (nodeWidth . fst . root) [last us,t]

-- svgTree{C} spread t computes SVG code for the tree of type PTree each of
-- whose node labels includes the position where the node is to be placed.

svgTree,svgTreeC :: Svg (Tree String)

svgTree spread t = (f pt,round2 $ treeSize pt)
	   where (_,h) = nodeSize $ root t
	         pt = mkPTree spread (20,20+h) t
		 f :: PTree -> String
		 f (V (a,p))    = concat $ nodes a p
	         f (F (a,p) ts) = concat $ map g ts++nodes a p
		                  where g t = line red p (snd $ root t)++f t
	         nodes a p = zipWith (drawBox rectangle col) ps ls
			     where (ps,ls,_,_) = textblock p text
			           (col,text) = case a of 'F':'F':b -> (white,b)
			              	                  _ -> (light yellow,a)

svgTreeC spread t = (f red pt,round2 $ treeSize pt)
    where pt = mkPTree spread (20,20) t
          f :: RGB -> PTree -> String
	  f col (V (a,p))    = concat $ nodes (light col) ps ls
	  		       where (ps,ls,_,_) = textblock p a
 	  f col (F (a,p) ts) = concat $ map g ts++nodes col' ps ls
            where g t = line next p (snd $ root t)++f next t
		  (col',text,next) = case a of 
			             'F':'F':b -> (white,b,col)
			             _ -> (light col,a,nextColor (height t) col)
	          (ps,ls,_,_) = textblock p text
	  nodes = zipWith . drawBox ellipse

svgTreeSub :: RGB -> Svg (Tree String,Node)
svgTreeSub col spread (t,sub) = (f [] pt,round2 $ treeSize pt)
   where (_,h) = nodeSize $ root t
	 pt = mkPTree spread (20,20+h) t
	 f :: Node -> PTree -> String
	 f pos (V (a,p))    = concat $ nodes a p pos
	 f pos (F (a,p) ts) = concat $ zipWithSucs g pos ts++nodes a p pos
                              where g pos t = line red p (snd $ root t)++f pos t
         nodes a p pos = zipWith (drawBox rectangle col') ps ls where
			 (ps,ls,_,_) = textblock p text
			 (col',text) = case a of 
			               'F':'F':b -> (white,b)
			               _  -> (lighter $ if sub <<= pos then col
			               			else yellow,a)

svgTreeSet :: Svg (Tree String,[Node])
svgTreeSet spread (t,poss) = (f [] pt,round2 $ treeSize pt)
   where pt = mkPTree spread (20,20) t
 	 f :: Node -> PTree -> String
	 f pos (V (a,p))    = concat $ nodes a p pos
	 f pos (F (a,p) ts) = concat $ zipWithSucs g pos ts++nodes a p pos
                              where g pos t = line red p (snd $ root t)++f pos t
         nodes a p pos' = zipWith (drawBox ellipse $ lighter col) ps ls where
		          (ps,ls,_,_) = textblock p a
			  col = if pos' `elem` poss then green else yellow

svgTreeRel :: Svg (Tree String,Node,[Node])
svgTreeRel spread (t,pos,poss) = (f [] pt,round2 $ treeSize pt)
   where pt = mkPTree spread (20,20) t
 	 f :: Node -> PTree -> String
	 f pos (V (a,p))    = concat $ nodes a p pos
	 f pos (F (a,p) ts) = concat $ zipWithSucs g pos ts++nodes a p pos
                              where g pos t = line red p (snd $ root t)++f pos t
         nodes a p pos' = zipWith (drawBox ellipse $ lighter col) ps ls where
		          (ps,ls,_,_) = textblock p a
		          b = pos' `elem` poss
			  col = if pos' == pos then if b then blue else red
			                       else if b then green else yellow

-- GRAPHS

p0 = (0,0)

-- straight p q r checks whether p, q and r form a line

straight :: Point -> Point -> Point -> Bool
straight (x1,y1) (x2,y2) (x3,y3) = x1 == x2 && x2 == x3 || 
                                   x1 /= x2 && x2 /= x3 &&
                                   (y2-y1)/(x2-x1) == (y3-y2)/(x3-x2)

minimize :: Path -> Path
minimize (p:ps@(q:r:s)) = if straight p q r then minimize $ p:r:s
                       			    else p:minimize ps
minimize ps = ps  

center,gravity :: Path -> Point
center ps        = ((x1+x2)/2,(y1+y2)/2) where (x1,y1,x2,y2) = minmax ps
gravity ps@(p:_) = apply2 (/(float $ length qs)) $ foldl1 add2 qs
                   where qs = if p == last ps then tail ps else ps
gravity ps       = error "emptyPath"

-- draw complex functions

type ComplexFun = Complex Float -> Complex Float
		
drawGrid :: ComplexFun -> IO ()
drawGrid f = do writeFile "grid" $ show $ mkGrid 50 f
	        readAndDraw 0 0 "grid" svgPlane $ Just . read
								     
mkGrid :: Int -> ComplexFun -> [CPoint]
mkGrid n f = zipWith h grid distances
             where coords = reverse [-n..(-1)]++[0..n]
	           grid = [float2 (x,y) | x <- coords, y <- coords]
                   distances = map g grid
		   g (x,y) = round $ distance (x,y) (a,b)
		                  -- angle 
		             where a:+b = f $ x:+y
		   maxdist = maximum distances
		   h (x,y) d = (hue 1 red maxdist d,x*4,y*4)
	   
svgPlane :: Point -> [CPoint] -> (String,Pos)
svgPlane (hor,ver) path = (code,size)
                   where (minx,miny,maxx,maxy) = minmax $ map coords path
		         coords :: CPoint -> Point
			 coords (_,x,y) = (x,y)
			 code = concatMap (drawCP . shift) path
	                 size = round2 $ add1 (g (maxx,maxy)) 20
			 shift (col,x,y) = (g (x,y),col)
			 drawCP (p,col) = rectangle col p (2,2)
			 g (x,y) = ((x-minx)*hor+40,(y-miny)*ver+20)
			 			 			    
zeta,eta,expc,two,quad,phylloc :: ComplexFun
zeta z@(x:+y)  = sum $ 1:[(float n:+0)**(-f x) | n <- [2..50]]
		 where f x = if x > 0 then z else (-x):+y
eta z@(x:+y)   = sum $ 1:[f (even n) $ (float n:+0)**(-g x) | n <- [2..50]]
                 where f b z = if b then -z else z
		       g x = if x > 0 then z else (-x):+y
expc z@(x:+y)  = z*exp(-f x) where f x = if x > 0 then z else (-x):+y
quad (x:+y)    = x*x:+y*y
two (x:+y)     = (x+x):+(y+y)
sico (x:+y)    = sin x:+cos y
phylloc (x:+y) = fst (f x):+snd (f y)
		 where f x = successor p0 (sqrt x) $ x*137.50776405003785

-- drawGraphI loads a graph g of type [CPath] resp. [CLPath] from a file, 
-- enters a loop that asks for scaling factors and an optional background and 
-- draws g into file.svg.

-- If color c has been assigned to path p, draw{L}Graph colors the i-th node of 
-- p with hue 1 c (length p) i (see section COLORS).

drawGraph,drawGraphI :: String -> IO ()
drawGraph file  = readAndDraw 1 1 file svgGraph $ Just . read
drawGraphI file = readAndDraw 0 0 file svgGraph $ Just . read

drawLGraph,drawLGraphI :: String -> Int -> IO ()
drawLGraph file mode  = readAndDraw 1 1 file (svgLGraph mode) $ Just . read
drawLGraphI file mode = readAndDraw 0 0 file (svgLGraph mode) $ Just . read
	   
svgGraph :: Svg [CPath]
svgGraph (hor,ver) paths = (code,size)
                     where (x1,y1,x2,y2) = minmax $ concatMap coords paths
		           coords :: CPath -> Path
			   coords (_,_,ps) = ps
		           code = concatMap (drawP . shift) paths
	                   size = round2 $ add1 (g (x2,y2)) 20
			   shift (col,mode,ps) = (col,mode,map g ps) 
			   g (x,y) = ((x-x1)*hor+40,(y-y1)*ver+20)

svgLGraph :: Int -> Point -> [CLPath] -> (String,Pos)
svgLGraph mode (hor,ver) paths = (code,size)
                  where (x1,y1,x2,y2) = minmax $ concatMap coords paths
                        coords :: CLPath -> Path
                        coords (_,lpath) = map (\(_,x,y) -> (x,y)) lpath
		        code = concatMap (drawLP mode . shift) paths
			size = round2 $ add1 (f (x2,y2)) 20
			shift (col,ps) = (col,[(lab,x',y') | (lab,x,y) <- ps, 
			                                 let (x',y') = f (x,y)])
			f (x,y) = ((x-x1)*hor+40,(y-y1)*ver+20)

showMode :: Int -> String
showMode mode = if lg < 5 then str++replicate (5-lg) '1' else take 5 str
	        where str = show mode
		      lg = length str  

drawP :: CPath -> String
drawP (col,mode,ps@(p:qs)) = edgesCode++nodesCode
    where is = indices_ ps
	  [nodeM,edgeM,b,colM,gradM] = showMode mode
          nodesCode = case nodeM of 
	  	      '1' -> ""
		      '2' -> nodes mkLight $ \c p -> ellipse c p (12,12)
		      '3' -> nodes mkLight $ \c p -> rectangle c p (12,12)
		      '4' -> drawBoxes c ps $ map show is
		      '5' -> nodes id $ \c p -> ellipse c p (6,6)
		      _ -> nodes id $ \c p -> rectangle c p (6,6)
          edgesCode = case edgeM of '1' -> ""
                                    '2' -> edges False
				    '3' -> edges True
	  			    '4' -> polyline smooth col ps
				    _ -> polygon smooth col ps
	  smooth = b > '1'
	  c = hue (read [colM]) col lg
	  nodes f code = concat $ zipWith (code . f . c) ks ps
	  edges tria = fst $ fold2 g ("",p) qs ks
	        where g (code,p) q i = (code++code',q)
	                where code' = if tria 
			              then polygon smooth (c i) [p,q,gravity ps]
	                              else line (c i) p q
          (ks,lg) = case gradM of '1' -> (is,lg1)
			          '2' -> sym
			          '3' -> h angle
				  _   -> h slope
                    where lg1 = length ps-1
		          lg2 = lg1`div`2
		          half = [0..lg2-1]
		          sym = if lg1`mod`2 == 0 then (half++reverse half,lg2)
			        else (half++lg2:reverse half,lg2+1)
		          h rel = (map g rels,length set)
	                   where f (is,p) q = (is++[rel p q],q)
			         rels = fst $ foldl f ([],p) qs
				 set = sort $ union [] rels
			         g rel = case search (== rel) set of Just i -> i
								     _ -> 0

drawLP :: Int -> CLPath -> String
drawLP mode (col,lpath) = polyline False col ps ++ drawBoxes c ps labs
	                  where (ps,labs) = unzip $ map f lpath
		                f (lab,p,q) = ((p,q),lab)
				c = hue mode col $ length ps

-- CURVES

data Curve = C {file :: String, colors :: [RGB], modes :: [Int],
		paths :: [Path]} deriving Show

drawC' :: (String -> IO ()) -> Curve -> IO ()
drawC' draw (C file cols modes paths) = 
                                do writeFile file $ show $ zip3 cols modes paths
				   draw file
				   
drawC   = drawC' drawGraph
drawCI  = drawC' drawGraphI

drawCS' :: Bool -> Float -> String -> [Curve] -> IO ()
drawCS' frame sc dir curves = 
                          do mkHtml frame dir n
		             drawDir sc sc dir (const svgGraph) (map f curves) n
                          where n = length curves
	                        f (C _ cols modes paths) = zip3 cols modes paths
drawCS  = drawCS' True 1 
drawCSI = drawCS' False 0

-- drawDirC dir act generates a list of curves by repeatedly executing act and 
-- stores them in the directory Pix/dir. For scrolling between the curves, 
-- call Pix/dir.html.						

drawDirC :: String -> IO Curve -> IO ()
drawDirC dir act = do b <- doesDirectoryExist pdir
                      when b $ removeDirectoryRecursive pdir
		      createDirectory pdir
		      scale 1
         where pdir = "Pix/" ++ dir
               write i = writeFile ("Pix/" ++ mkFile dir i) . embed ""
               scale i = getScaleAndAct act' $ scale $ i+1 where
               		 act' hv _ = do mkHtml False dir i
			                C _ cs ms paths <- act
			                write i $ svgGraph hv $ zip3 cs ms paths

isEmpty = null . paths
		   
emptyC file = C file [] [] []

single :: String -> RGB -> Int -> Path -> Curve
single file col mode ps = C file [col] [mode] [ps]
		
mapCurve :: (Point -> Point) -> Curve -> Curve
mapCurve f c = c {paths = map (map f) $ paths c}

updFile :: String -> Curve -> Curve
updFile file c = c {file = file}

updCol :: RGB -> Curve -> Curve
updCol col c  = c {colors = take (length $ colors c) $ repeat col}

hueCol :: Int -> Curve -> Curve
hueCol mode c = c {colors = zipWith f (colors c) $ indices_ ps}
	        where ps = paths c
		      f col = hue mode col $ length ps

chgCol :: RGB -> RGB -> Curve -> Curve
chgCol old new c  = c {colors = map f $ colors c}
		    where f col = if col == old then new else col
			    
shift :: Point -> Curve -> Curve
shift (a,b) c = c {paths = map (map $ \(x,y) -> (x+a,y+b)) $ paths c}

scale,hscale,vscale,turn :: Float -> Curve -> Curve
scale a c  = c {paths = map (map $ apply2 (a*)) $ paths c} 
hscale a c = c {paths = map (map $ \(x,y) -> (a*x,y)) $ paths c} 
vscale a c = c {paths = map (map $ \(x,y) -> (x,a*y)) $ paths c} 
turn a c   = turn0 a (gravity $ concat $ paths c) c

turn0 :: Float -> Point -> Curve -> Curve
turn0 a p c = c {paths = map (map $ \q -> rotate p a q) $ paths c} 

updMod,shiftCol,takeC,dropC :: Int -> Curve -> Curve
updMod mode c = c {modes = take (length $ modes c) $ repeat mode}
shiftCol n c  = c {colors = map ((!!n) . iterate nextCol) $ colors c}
takeC n c     = c {paths = map (take n) $ paths c}
dropC n c     = c {paths = map (drop n) $ paths c}

complCol,flipH,flipV,transpose,toCenter :: Curve -> Curve
complCol   = shiftCol 765
flipH      = mapCurve $ \(x,y) -> (x,-y)
flipV      = mapCurve $ \(x,y) -> (-x,y)
transpose  = mapCurve $ \(x,y) -> (y,x)
toCenter c = shift (apply2 negate $ gravity $ concat $ paths c) c
toStart c  = shift (apply2 negate $ head $ head $ paths c) c

combine :: [Curve] -> Curve
combine cs@(c:_) = c {colors = f colors, modes = f modes, paths = f paths} 
                   where f = flip concatMap cs
combine _        = emptyC ""

overlay = combine . map toCenter

inCenter :: (Curve -> Curve) -> Curve -> Curve
inCenter f c = shift (gravity $ concat $ paths c) $ f $ toCenter c

morphing :: Int -> Int -> [Curve] -> Curve
morphing m n cs = combine $ concat $ zipWith f (init cs) $ tail cs
 where f c d = map g [0..n]
	       where g i = c {paths = zipWith h (paths c) $ paths d,
		              colors = map hc $ colors c}
	                   where h ps qs = zipWith morph ps' qs'
		                           where diff = length qs-length ps
					         ps' = adaptLength ps diff
					         qs' = adaptLength qs $ -diff
			         morph (xc,yc) (xd,yd) = (next xc xd,next yc yd)
			         next x z = (1-inc)*x+inc*z
			         inc = float i/float n
				 hc col = hue m col n i 

transform :: (Int -> RGB -> Int -> Int -> RGB) -> Int -> Int -> Curve -> Curve
transform t m n c = combine $ map f $ indices_ pss
                    where pss = paths c 
			  f i = c {colors = map g [0..n-1],
		                   modes = replicate n $ modes c!!i,
			           paths = map h [0..n-1]}
				where g k = t m (colors c!!i) n k
				      h k = map (\(x,y) -> (a*x,a*y)) $ pss!!i
	                                    where a = float (n-k)/float n

rainbow = transform hue
shine   = transform $ \m col n i -> lights col n $ m*i

rect :: RGB -> Int -> Point -> Curve
rect col mode p@(b,h) = single "rect" col mode [p,(b,-h),(-b,-h),(-b,h),p]
		
arc :: RGB -> Int -> Int -> [Float] -> Int -> Curve
arc col mode n rs k = single "poly" col mode $ last ps:ps
	              where ps = fst $ foldl f ([],0) $ take k $ cycle rs
			    f (ps,a) 0 = (ps,a+inc)
			    f (ps,a) r = (successor p0 r a:ps,a+inc)
			    inc = 360/float (n*length rs)
			    
poly :: RGB -> Int -> Int -> [Float] -> Curve
poly col mode n rs = arc col mode n rs $ n*length rs 

tria,circ :: RGB -> Int -> Float -> Curve
tria col mode r = turn 30 $ poly col mode 3 [r]
circ col mode r = poly col mode 100 [r]

elli :: RGB -> Int -> Float -> Float -> Curve
elli col mode a b = vscale (b/a) $ circ col mode a

piles :: RGB -> Int -> [Int] -> Curve
piles col mode s = single "piles" col mode $ coords s 0
	          where maxheight = 24*(length s-1)
	                coords (n:s) x = mkPile n x maxheight ++ coords s (x+24)
	                coords _ _     = []
	           	mkPile 0 _ _ = []
	           	mkPile n x y = float2 (x,y):mkPile (n-1) x (y-24)

cant,snake,phyllo :: RGB -> Int -> Int -> Curve
cant col mode n = single "cant" col mode $ map (float2 . p) [0..n*n-1]
	          where p 0 = (0,0)
	    	        p i = if even x 
		              then if even y
			           then if x > 0 
			                then if y' < n then (x-1,y') else (x',y)
					else if y' < n then (0,y') else (1,y) 
			           else if x' < n then (x',y-1) else (x,y') 
		              else if even y
			           then if y > 0 
			                then if x' < n then (x',y-1) else (x,y') 
					else if x' < n then (x',0) else (x,1)
			           else if y' < n then (x-1,y') else (x',y) 
		              where (x,y) = p $ i-1; x' = x+1; y' = y+1
			  
snake col mode n  = single "snake" col mode $ map (float2 . p) [0..n*n-1]
	            where p i = (if even yi then xi else n-1-xi,yi)
			        where xi = i`mod`n; yi = i`div`n
			    
phyllo col mode n = single "phyllo" col mode $ map f [1..n]
		    where f i = successor p0 (sqrt k) $ k*137.50776405003785
			        where k = float i

leaf :: RGB -> RGB -> Int -> Float -> Int -> Curve
leaf col col' mode r k = combine [toStart $ c col,toStart $ flipH $ c col']
                         where a = 0.18*(501-float k)
		               c col = turn a $ arc col mode 1000 [r] k
		
blosLeaf :: RGB -> Int -> Bool -> Int -> [Int] -> Curve
blosLeaf col mode double n ks = turtle col 0 $ map (Put . toStart) cs 
   where lg = n*length ks
         (cs,_,_) = foldl f ([],col,0) $ take lg $ cycle ks
	 f (cs,col,a) 0 = (cs,col,a+inc)
         f (cs,col,a) k = (turn a (leaf col col' mode 100 k):cs,next col',a+inc)
			  where col' = if double then next col else col
         next = if double then nextColor $ 2*lgc else nextColor lgc
	 lgc = n*length (filter (/= 0) ks)
	 inc = 360/float lg
		
blosCurve :: RGB -> Int -> (RGB -> Curve) -> Curve
blosCurve col n c = turtle col 0 $ map (Put . toStart) cs 
                  where (cs,_,_) = iterate f ([],col,0)!!n
		        f (cs,col,a) = (turn a (c col):cs,nextColor n col,a+inc)
		        inc = 360/float n

-- knight paths (Springer-Wege): iterative version

kpath :: RGB -> Int -> Int -> Float -> Float -> Curve
kpath col mode n a b = case pathloop n (a,b) of 
                            Just ps -> single "kpath" col mode ps
			    _ -> emptyC "kpath"
			
pathloop :: Int -> Point -> Maybe Path
pathloop n p = f (n*n) [p] [nextPoss n [] p]
	   where f 1 ps _            = Just $ reverse ps
	         f k ps ((p:qs):pss) = if p `elem` ps then f k ps qss
			               else f (k-1) (p:ps) $ nextPoss n ps p:qss
			               where qss = qs:pss
                 f k ps pss          = do _:ps <- Just ps; _:pss <- Just pss
			   		  f (k+1) ps pss

nextPoss,newSucs :: Int -> Path -> Point -> Path
nextPoss n = sortVal . newSucs n
newSucs n visited = filter (`notElem` visited) . sucs n

sucs :: Int -> Point -> Path
sucs n p = [q | q@(a,b) <- incrs p, 0 < a, a <= float n, 0 < b, b <= float n]
		    
incrs :: Point -> Path
incrs (x,y) = [(x+1,y+2),(x+1,y-2),(x-1,y+2),(x-1,y-2),
	       (x+2,y+1),(x+2,y-1),(x-2,y+1),(x-2,y-1)]
	      --[(x+1,y),(x,y-1),(x-1,y),(x-1,y),
	      -- (x+1,y+1),(x+1,y-1),(x-1,y+1),(x-1,y-1)]
		    
sortVal :: (Point -> Path) -> Point -> Path
sortVal f p = sort $ f p
   where sort (x:s) = sort [y | y <- s, r y x]++x:sort [y | y <- s, not $ r x y]
         sort s     = s
         r p q = length (f p) < length (f q)

-- knight tours (Springer-Kreise)

ktour :: RGB -> Int -> Int -> Float -> Float -> Curve
ktour col mode n a b = case tourloop n (n*n) [p] [nextPoss n [] p] of 
		     	    Just ps -> single "ktour" col mode ps
			    _ -> emptyC "ktour"
		       where p = (a,b)

tourloop :: Int -> Int -> Path -> [Path] -> Maybe Path
tourloop n 1 ps@(p:qs) (_:pss) = if q `elem` sucs n p then Just $ reverse $ q:ps 
						      else tourloop n 2 qs pss
	          	         where q = last ps            -- = init
tourloop n k ps ((p:qs):pss)   = if p `elem` ps then tourloop n k ps qss
           		         else tourloop n (k-1) (p:ps) $
	                                       nextPosst n ps (last ps) p:qss
			         where qss = qs:pss           -- last ps = init
tourloop n k ps pss            = do _:ps <- Just ps; _:pss <- Just pss
				    tourloop n (k+1) ps pss

nextPosst :: Int -> Path -> Point -> Point -> Path
nextPosst n visited init p
       | init `notElem` sucs n p = case singlesuc fp of
	                           Just i -> case singlesuc $ context fp i of
			                     Just _ -> []   		-- (c)
					     _ -> [fp!!i]		-- (b)
                                   _ -> sortVal f p			-- (d)
       | null $ f init 	         = []                    		-- (a)
       | True 		         = sortVal f p			        -- (d)
    			           where f = newSucs n visited
			                 fp = f p
				         singlesuc = search ((==1) . length . f)
       -- (a)-(d) implement the synonymous rules of 
       -- Rabhi, Lapalme, Algorithms, Addison-Wesley 1999, Exercise 8.7.

-- snowflakes

-- snow huemode mode d n k c computes a Koch snowflake with turn mode m in 
-- {1,..,6}, depth n and the k copies of scale(i/3)(c) at level 
-- 1<=i<=n revolving around each copy of scale((i-1)/3)(w) at level i-1. 
-- The real number d should be taken from the closed interval [-1,1]. 
-- d affects the radia of the circles consisting of k copies of w.

snow :: Int -> Int -> Float -> Int -> Int -> Curve -> Curve
snow huemode mode d n k c = 
     if n <= 0 || isEmpty c then emptyC $ file c else color $ combine $ c:cs
     where color = if huemode < 4 then id else hueCol $ huemode-3
           ps = concat $ paths c
           a = 360/float k
	   cs = g (n-1) [p0] $ maximum $ map (distance $ gravity ps) ps
	   g :: Int -> Path -> Float -> [Curve]
	   g 0 _ _  = []
	   g i ps r = zipWith3 h qs angs flips++g (i-1) qs r'
             where qs = concatMap circle ps
	           r' = r/3
		   circle p@(x,y) = if odd mode then s else p:s
		            where s = take k $ iterate (rotate p a) (x,y-r+d*r')
		   f col = hue huemode col n $ n-i
		   h p a b = shift p $ turn a $ scale (b/3^(n-i)) c'
		   c' = if huemode < 4 then c {colors = map f $ colors c} else c
           angs | mode < 5  = zeros 
	        | mode == 5 = iter++angs
		| True      = 0:iter++concatMap f [1..n-2]
		              where f i = concatMap (g i) iter 
		      	            g 1 a = a:iter
				    g i a = g k a++f k where k = i-1
	   iter  = take k $ iterate (+a) 0
	   zeros = 0:zeros
	   ones  = 1:ones
	   blink = 1: -1:blink
	   flips = case mode of 1 -> blink
	                        2 -> 1:take k blink++flips
				_ -> ones

-- turtle-generated curves

data Action = Turn Float | Move Float | Jump Float | Put Curve | 
	      Open RGB Float Bool | Close | Draw | Skip deriving Show
{-
data Action where Turn,Move,Jump :: Float -> Action
		  Put :: Curve -> Action 
		  Open :: RGB -> Float -> Bool -> Action
		  Close,Draw,Skip :: Action
		  deriving Show
-}	      
go   = Move 90 
up   = Turn (-90)
down = Turn 90
	               
triaA  = [Turn (-60),go,Turn 120,go,Turn 120,go] 
pentaA = Turn (-108):go:concat (replicate 4 [Turn 72,go])
hexaA  = Turn (-120):go:concat (replicate 5 [Turn 60,go])

pentaS = [Turn (-99),go,Turn 66,Move 70,Turn 66,Move 70,Turn 66,go,Turn 81,go]

rhombLB = [Turn (-108),go,Turn 108,go,Turn 72,go,Turn 108,go] 	
rhombRB = [Turn (-72),go,Turn 72,go,Turn 108,go,Turn 72,go]   
rhombLS = [Turn (-144),go,Turn 144,go,Turn 36,go,Turn 144,go] 
rhombRS = [Turn (-36),go,Turn 36,go,Turn 144,go,Turn 36,go]   

mkActs :: Path -> [Action]
mkActs = f 0 where f a (p:ps@(q:_)) = Turn (b-a):Move (distance p q):f b ps
	  		     	      where b = angle p q
                   f _ _ = []

turtle :: RGB -> Int -> [Action] -> Curve
turtle col mode acts = case foldl f iniState acts of 
			    (c,(_,_,ps,col,b):_) -> g c ps col b
			    _ -> emptyC "turt"
      where iniState = (emptyC "turt",[(0,1,[p0],col,smooth)])
	    f st@(c,states@((a,sc,ps,col,b):s)) act =
	        case act of Jump d          -> (g c ps col b,(a,sc,[q],col,b):s)
				               where q = successor p (d*sc) a
                            Move d          -> (c,(a,sc,ps++[q],col,b):s)
			                       where q = successor p (d*sc) a
			    Turn a'         -> (c,(a+a',sc,ps,col,b):s)
			    Skip            -> st
			    Open col sc' b' -> (c,(a,sc*sc',[p],col,b'):states)
			    Close           -> (g c ps col b,s)
			    Draw            -> (g c ps col b,(a,sc,[p],col,b):s)
			    Put c'          -> (h c',states)
	        where p = last ps
		      h c' = combine [c,turn0 a p $ shift p $ scale sc c']
	    g c ps col b = if length ps < 2 then c 
		 	   else combine [c,single "turt" col (mode' b) ps]
	    str = showMode mode
	    smooth = str!!2 == '2'
	    mode' b = if b && not smooth then read $ updList str 2 '2' else mode
					   
data Direction = North | East | South | West

north,east,south,west :: [Action]
north = [up,Move 5,down]
east  = [Move 5]
south = [down,Move 5,up]
west  = [Move $ -5]

hilbertActs :: Int -> Direction -> [Action]
hilbertActs 0 = const []
hilbertActs n = \case East -> hSouth++east++hEast++south++hEast++west++hNorth
                      West -> hNorth++west++hWest++north++hWest++east++hSouth
                      North -> hWest++north++hNorth++west++hNorth++south++hEast
                      South -> hEast++south++hSouth++east++hSouth++north++hWest
                where h = hilbertActs (n-1); hEast = h East; hWest = h West
                      hNorth = h North; hSouth = h South
		 
hilb :: RGB -> Int -> Int -> Curve
hilb col mode = turtle col mode . flip hilbertActs East

fibo :: RGB -> Int -> Int -> Float -> Float -> Curve
fibo col mode n a b  = turtle col mode $ concatMap f $ take n fib
                       where fib  = concat fibs
		             fibs = [0]:zipWith (++) fibs ([1]:fibs)
			     f 0 = [Turn a,Move 5]
			     f _ = [Turn (-b),Move 5]
			 
bush,dragon,dragonF,fern,fernL,fernU,gras,grasL :: RGB -> Int -> Int -> Curve

bush col mode n = turtle col mode $ up:f n
  	          where f 0 = [Move 3]
                        f i = acts++acts++open:Turn 20:acts++acts++Close:
                              open:Turn (-20):acts++Turn 20:acts++[Close]
			      where acts = f (i-1)
	                open = Open col 1 True

bushL col mode n = turtle col mode $ up:f n col
  	           where f 0 _   = [Move 3]
                         f i col = acts++acts++open:Turn 20:acts++acts++Close:
                                   open:Turn (-20):acts++Turn 20:acts++[Close]
			           where acts = f (i-1) $ nextColor n col
	                                 open = Open col 1 True

dragon col mode n = turtle col mode $ f n
  	            where f 0 = [Move 10]
                          f i = Turn 45:f (i-1)++Turn (-135):g (i-1)++[Turn 45]
			  g 0 = [Turn 45,Move 10]
			  g i = f (i-1)++Turn 45:g (i-1)++[Turn (-45)]

dragonF col mode n = flipH $ turtle col mode $ concatMap f (take n drag)
	             where drag  = zip blink drag
		           blink = 0:1:blink
			   zip (x:s) t = x:zip t s
			   f 0 = [up,Move 5]
			   f _ = [down,Move 5]
			
fern col mode n = turtle col mode $ up:f n 1
            where f 0 _ = [Move 10]
                  f i a = g 0.35 (a*50) (-a)++g 0.35 (-a*50) a++g 0.85 (a*2.5) a
	                  where g sc a b = Move 10:Open col sc False:Turn a:
		                           f (i-1) b++[Close]
 
fernL col mode n = turtle col mode $ up:f n 1 col
        where f 0 _ _   = [Move 10]
              f i a col = g 0.35 (a*50) (-a)++g 0.35 (-a*50) a++g 0.85 (a*2.5) a
	                  where g sc a b = Move 10:Open col sc False:Turn a:
		                           f (i-1) b (nextColor n col)++[Close]
 
fernU col mode n = turtle col mode $ up:f n 0
  	           where f 0 _ = []
                         f n 0 = act:open:Turn 30:g 1++Close:
		                 open:Turn (-30):g 1++Close:act:g 0
	                         where act = Move $ 1.5^(n-1)
		                       g = f (n-1)
                         f n k = f (n-1) $ k-1
			 open = Open col 1 False
 
fernUL col mode n = turtle col mode $ up:f n 0 col
  	            where f 0 _ _   = []
                          f n 0 col = act:open:Turn 30:g 1++Close:
		                      open:Turn (-30):g 1++Close:act:g 0
	                              where act = Move $ 1.5^(n-1)
		                            g k = f (n-1) k $ nextColor n col
					    open = Open col 1 False
                          f n k col = f (n-1) (k-1) $ nextColor n col

gras col mode n = turtle col mode $ up:f n
  	          where f 0 = [Move 6]
                        f i = m:open False:h (-25)++m:open False:h 37.5++
	                      open True:m:h 12.5 
			      where m = Move $ 2^i 
			            h a = Turn a:f (i-1)++[Close]
		        open = Open col 1

grasL col mode n = turtle col mode $ up:f n col
  	           where f 0 _   = [Move 6]
                         f i col = m:open False:h (-25)++m:open False:h 37.5++
	                           open True:m:h 12.5 
			   where m = Move $ 2^i 
			         h a = Turn a:f (i-1) (nextColor n col)++[Close]
				 open = Open col 1

grasC :: RGB -> Int -> Int -> Curve -> Curve
grasC col mode n c = turtle col mode $ open 6:up:f n++[Close]
   	            where f 0 = [Put $ scale 0.2 c]
                          f i = m:up:open 1:acts++Close:open 1:down:acts++Close:
		                down:m:open 1:down:m:acts++Close:up:acts
	                        where acts = f $ i-1;    m = Move $ 2^i
                                      up = Turn $ -22.5; down = Turn 22.5	
	                  open sc = Open col sc False
		     
rhombH col = single "" col 15211 [p0,(3,-2),(16,0),(3,2),p0]

bunch :: Int -> Curve
bunch n = turtle red 12111 $ if n <= 0 then []
	  			       else up:f (-60)++f (-30)++f 0++f 30++f 60
          where f a = [Open red 1 False,Turn a,go,Put $ poly red 13111 6 [6,12],
	               down,Open red 0.75 False,Put $ bunch $ n-1,Close,Close]

-- growC c f branches docks the based curves given by the actions of branches 
-- with their respective base lines ((90,0),p0) at f(c).
				
growC :: Curve -> (Curve -> Curve) -> [[Action]] -> [Action]
growC c f branches = Put (f c):
		     concat (zipWith g branches $ mkLines $ concat $ paths c)
        where g [] _               = []
              g branch (p@(x,y),q) = Open red 1 False:Jump x:down:Jump y:Turn a:
	   			     Open red (d/90) False:branch++[Close,Close]
				     where a = angle p q-90; d = distance p q
						 
grow :: Curve -> (Curve -> Curve) -> [Curve] -> Curve
grow c f = turtle red 1 . growC c f. map (\c -> if isEmpty c then [] 
							     else [Put c])
						       
growR :: Path -> [Bool] -> (Curve -> Curve) -> RGB -> Int -> Int -> Curve
growR ps bs f col mode n = turtle red 1 $ g n col 
                        where g 0 _   = []
		              g i col = growC (C "" [col] [mode] [ps]) f
			      		      $ map h bs
		                        where h True = g (i-1) $ nextColor n col
				      	      h _    = []

-- based curves
	       
triaP   = [p0,(45,-77.94229),(90,0),p0]
square  = [p0,(0,-90),(90,-90),(90,0),p0]
penta   = [p0,(-27.811533,-85.595085),(45,-138.49576),(117.81153,-85.595085),
	   (90,0),p0]
hexa    = [p0,(-45,-77.94228),(0,-155.88458),(90,-155.88458),(135,-77.94229),
	   (90,0),p0]
trunk   = [p0,(0,-90),(36,-135),(90,-90),(90,0),p0]
cactus  = [p0,(7.5,-60),(22.5,-90),(67.5,-90),(82.5,-60),(90,0),p0]	

rhomb n = p0:ps++[(90,0),p0]
          where ps = case n of 1 -> [(-x1,-x2),(62.18847,-x2)]        -- rhombLB
                    	       2 -> [(27.811527,-x2),(117.81152,-x2)] -- rhombRB
			       3 -> [(-72.81154,-x3),(17.188461,-x3)] -- rhombLS
		       	       _ -> [(72.81153,-x4),(162.81152,-x4)]  -- rhombRS
                x1 = 27.811533; x2 = 85.595085; x3 = 52.900665; x4 = 52.900673

base :: Bool -> Curve -> Curve
base inner c = if null pss || length ps < 3 || p /= last ps || d == 0 then c 
               else c {paths = rs:rss}
	     where pss@(ps@(p:_):qss) = paths c
	       	   (rs,d,a) = basedCurve
		   rss = map (mkBased d a p) qss
                   basedCurve = (mkBased d a p rs,d,a)
 		              where rs@(p:qs) = if inner then ps else reverse ps
		                    q = last $ init qs
				    d = distance p q
				    a = -angle p q
		   mkBased :: Float -> Float -> Point -> Path -> Path
		   mkBased d a p = map $ apply2 (*(90/d)) . rotate p0 a . sub2 p
					    
-- repeat based curves 

trias   = growR triaP [False,True,True]
squares = growR square $ replicate 5 True		 
pentas  = growR penta $ replicate 5 True
hexas   = growR hexa $ replicate 5 True
pytree  = growR trunk [False,True,True]

-- action growing (probably superfluous)

growA :: RGB -> [Action] -> [[Action]] -> [Action]
growA col acts branches = Open col 1 False:acts++Close:f acts branches
                where f (turn:Move d:acts) (b:bs) = turn:acts'++Jump d:f acts bs
	    	             where acts' = if null b then []
	    			           else Open col (d/90) False:b++[Close]
	              f _ _ = []

growAM :: [Action] -> [Bool] -> RGB -> Int -> Int -> Curve
growAM acts bs col mode n = turtle red mode $ f n col 
                        where f 0 _   = []
                              f i col = growA col acts $ map g $ bs
		           	        where g True = f (i-1) $ nextColor n col
			                      g _    = []
					      
pytreeA = growAM (mkActs trunk) [False,True,True]


-- GRAPHIC PRIMITIVES

type Point2D = (Int,Int)

angle :: RealFloat a => (a,a) -> (a,a) -> a
angle (x1,y1) (x2,y2) = f (y2-y1) (x2-x1)*180/pi where f 0 0 = atan2 0 1
			      			       f x y = atan2 x y

slope :: Point -> Point -> Float
slope (x1,y1) (x2,y2) = if x1 == x2 then float maxBound else (y2-y1)/(x2-x1) 
			      
-- successor p (distance p q) (angle p q) = q. 

successor :: Point -> Float -> Float -> Point
successor (x,y) r a = (x+r*c,y+r*s) where (s,c) = sincos a
				 -- successor p 0 _ = p
				 -- successor (x,y) r 0 = (x+r,y) 
				 -- successor (x,y) r a = rotate (x,y) a (x+r,y)

sincos a = (sin rad,cos rad) where rad = a*pi/180 	-- sincos 0 = (0,1)

distance :: Point -> Point -> Float
distance (x1,y1) (x2,y2) = sqrt $ (x2-x1)^2+(y2-y1)^2

perimeter :: Path -> Float
perimeter ps = if peri <= 0 then 0.01 else peri
               where peri = sum $ zipWith distance ps $ tail ps
	        
mkLines :: Path -> [(Point,Point)]
mkLines ps = zip ps $ tail ps 
		
addPoints :: Path -> [Float] -> Path
addPoints ps []               = ps
addPoints (p:ps@(q:_)) (d:ds) = if d > d' then p:addPoints ps (d-d':ds)
 		                else p:addPoints (successor p d a:ps) ds
		                where d' = distance p q; a = angle p q
addPoints _ _ = error "addPoints"
	
adaptLength :: Path -> Int -> Path
adaptLength ps n = if n > 0 then addPoints ps $ dps/2:replicate (n-1) dps
                            else ps
                   where dps = perimeter ps/k; k = float n

-- rotate q a p rotates p clockwise by a around q on the axis (0,0,1).
-- rotate (0,0) 45 (5,5) --> (0.0,7.071068) = (0.0,sqrt $ 5^2+5^2)

rotate :: Point -> Float -> Point -> Point
rotate _ 0 p             = p 			
rotate q@(i,j) a p@(x,y) = if p == q then p else (i+x1*c-y1*s,j+x1*s+y1*c)
			   where (s,c) = sincos a; x1 = x-i; y1 = y-j

	
-- rotateA q (a,nx,ny,nz) p rotates p clockwise by a around q on the axis
-- (nx,ny,nz).

-- rotateA p0 45 (0,0,1) (5,5) --> (0.0,7.071068)
-- rotateA p0 45 (1,0,0) (5,5) --> (5.0,3.535534)
-- rotateA p0 45 (0,1,0) (5,5) --> (3.535534,5.0)

rotateA :: Point -> Float -> (Float,Float,Float) -> Point -> Point
rotateA _ 0 _ p 		     = p
rotateA q@(i,j) a (nx,ny,nz) p@(x,y) = if p == q then p 
				       else (f i (c'*nx*nx+c) (c'*nx*ny-s*nz),
      		                             f j (c'*nx*ny+s*nz) (c'*ny*ny+c))
                                       where (s,c) = sincos a; c' = 1-c
                                             f i a b = i+(x-i)*a+(y-j)*b

spline :: Path -> Path
spline ps@(p:_:_:_) = if p == last ps then spline0 True $ init ps
				      else spline0 False ps
spline ps = ps
					  
-- spline0 b ps uses ps as control points for constructing a closed (b = True) 
-- resp. open (b = False) B-spline with degree 3; see Paul Burke, Spline Curves
-- (http://astronomy.swin.edu.au/~pbourke/curves/spline)
-- or Heinrich MŸller, B-Spline-Technik, Vorlesung Geometrisches Modellieren 
-- (http://ls7-www.cs.tu-dortmund.de).

spline0 :: Bool -> Path -> Path
spline0 b ps = first:map f [1..resolution] ++ map g [1..9] ++
	       [if b then first else ps!!(n-1)]
         where first = f 0; n = length ps; resolution = n*6
	       f i = point $ upb*float i/float resolution 
               g i = point $ upb+float i/10
	       upb = float n-if b then 1 else 3
	       point v = foldl1 add2 $ map h [0..n-1] 
	                 where h i = apply2 (*z) $ ps!!i
	                         where z | b && v < u i = blend2 u i $ v-u 0+u n
		                         | b 	        = blend2 u i v
		     	                 | True         = blend2 t i v 
               t i = if i < 3 then 0 else float (min i n)-2
	       u i = if i <= n then float i else u (i-1)+u (i-n)-u (i-n-1)
	       h d s = if d == 0 then 0 else s
	       blend2 t i v = h denom1 sum1+h denom2 sum2
	       		      where ti = t i; ti3 = t $ i+3
		                    denom1 = t (i+2)-ti;  num1 = v-ti
				    denom2 = ti3-t (i+1); num2 = ti3-v
				    sum1 = num1/denom1*blend1 t i v
				    sum2 = num2/denom2*blend1 t (i+1) v
               blend1 t i v = h denom1 sum1+h denom2 sum2
		              where ti = t i; ti1 = t $ i+1; ti2 = t $ i+2
		                    denom1 = ti1-ti;  num1 = v-ti 
				    denom2 = ti2-ti1; num2 = ti2-v
				    sum1 = if b i then num1/denom1 else 0
				    sum2 = if b $ i+1 then num2/denom2 else 0
				    b i = t i <= v && v < t (i+1)

-- COLORS

data RGB = RGB Int Int Int deriving Eq

red     = RGB 255 0 0
green   = RGB 0 255 0
blue    = RGB 0 0 255
magenta = RGB 255 0 255
cyan    = RGB 0 255 255
yellow  = RGB 255 255 0 
orange  = RGB 255 180 0
brown   = RGB 0 160 255
black   = RGB 0 0 0
white   = RGB 255 255 255
grey    = RGB 100 100 100

lights :: RGB -> Int -> Int -> RGB
lights (RGB x y z) n i = RGB (f x) (f y) (f z)
             where f x | i > 0 = x+i*(255-x)`div`n -- i=n  --> f x = 255 (white)
		       | True  = x+i*x`div`n       -- i=-n --> f x = 0   (black)

light col = lights col 2 1 

lighter col = lights col 3 2 
			  
mkLight :: RGB -> RGB
mkLight c = if c == black then white else light c

-- nextCol computes the successor of each color c c on a chromatic circle of 
-- 1530 equidistant pure (or hue) colors. A color c is pure if c is neither
-- black nor white and at most one of the R-, G- and B-values of c is different
-- from 0 and 255.

nextCol,complColor :: RGB -> RGB
 
nextCol = \case RGB 255 n 0 | n > 0   -> RGB 255 (n-1) 0  -- n = 255 --> yellow
		RGB 255 0 n | n < 255 -> RGB 255 0 (n+1)  -- n = 0   --> red
		RGB n 0 255 | n > 0   -> RGB (n-1) 0 255  -- n = 255 --> magenta
		RGB 0 n 255 | n < 255 -> RGB 0 (n+1) 255  -- n = 0   --> blue 
		RGB 0 255 n | n > 0   -> RGB 0 255 (n-1)  -- n = 255 --> cyan 
		RGB n 255 0 | n < 255 -> RGB (n+1) 255 0  -- n = 0   --> green
		c  		      -> c 

hoch :: (a -> a) -> Int -> a -> a                             
f`hoch`n = if n == 0 then id else f . (f`hoch`(n-1))

complColor = nextCol`hoch`765
-- complColor c = iterate nextCol c!!765
 
-- hue mode col n i computes the i-th successor of col in a chromatic circle of 
-- n <= 1530 pure colors.

hue :: Int -> RGB -> Int -> Int -> RGB
hue 1 col n i = iterate nextCol col!!round (float i*1530/float n) 
hue 2 col n 0 = col
hue 2 col n i = if odd i then complColor $ hue 2 col n $ i-1
                         else nextColor (n `div` 2) $ hue 2 col n $ i-2
hue 3 col n i = if odd i then complColor d else d where d = hue 1 col n i
hue _ col _ _ = col

nextColor :: Int -> RGB -> RGB
nextColor n col = hue 1 col n 1

instance Read RGB where 
              readsPrec _ s1 = 
                    [(RGB r g b,s5) | ("RGB",s2) <- lex s1,(r,s3) <- reads s2,
    		                      (g,s4) <- reads s3, (b,s5) <- reads s4] ++
                    [(rgb c,s2) | (c,s2) <- lex s1, c `elem` 
                         words "red green blue yellow magenta cyan black white"]
		  
rgb "red"    = red;    rgb "green"   = green;   rgb "blue" = blue
rgb "yellow" = yellow; rgb "magenta" = magenta; rgb "cyan" = cyan
rgb "black"  = black;  rgb "white"   = white

instance Show RGB where 
              showsPrec _ (RGB x y z) = ("RGB "++) . shows x . (' ':) . shows y
						             . (' ':) . shows z

-- SVG-Code

embed :: String -> (String,Pos) -> String
embed "" (code,p)   = svgHead p ++ {-border p ++-} code ++ "</svg>"
embed back (code,p) = svgHead p ++ "\n<image xlink:href=\"" ++ back ++ '\"':
		      svgFrame p ++ "/>" ++ {-border p ++-} code ++ "</svg>"
-- "\n"++ code ++ "<image xlink:href=\""++ back ++ '\"':svgFrame p ++ "/></svg>"

svgHead (x,y) = "<svg xmlns"&"http://www.w3.org/2000/svg" ++
	        "\nxmlns:xlink"&"http://www.w3.org/1999/xlink" ++
		"\nversion"&"1.1" ++ " baseProfile"&"full" ++
		"\nwidth"&show (x+20) ++ " height"&show (y+20) ++ ">"

svgFrame (x,y) = " x"&"5"++" y"&"5"++" width"&show (x+10)++" height"&show (y+10)

-- border p = "\n<rect" ++ svgFrame p ++ " fill"&"none" ++ " stroke"&
--            "rgb(100,100,100)" ++ " stroke-width"&"1.5" ++ "/>"

rectangle :: RGB -> Point -> Point -> String
rectangle col (x,y) (rx,ry) = 
		             polygon False col [(x1,y1),(x2,y1),(x2,y2),(x1,y2)]
                             where x1 = x-rx; x2 = x+rx; y1 = y-ry; y2 = y+ry
		    
drawBox :: (RGB -> Point -> Point -> String) -> RGB -> Point -> String -> String
drawBox f col p@(x,y) a = if null a then "" 
			  else f col p (width+2,12) ++ 
			       text black (x-width,y+5) (concatMap g a)
                          where width = if a == "&#955;" then 5 else lgW a
                                g '\\' = "&#92;"
                                g c    = [c]

drawBoxes :: (Int -> RGB) -> Path -> [String] -> String
drawBoxes c ps = concat . zipWith3 (drawBox ellipse . mkLight . c) [0..] ps

ellipse :: RGB -> Point -> Point -> String
ellipse (RGB r g b) (x,y) (rx,ry) = "\n<ellipse cx"&show x  ++ " cy"&show y ++ 
				              " rx"&show rx ++ " ry"&show ry ++ 
					      " fill"&("rgb"++show (r,g,b)) ++
				    "/>"

line :: RGB -> Point -> Point -> String
line (RGB r g b) (x1,y1) (x2,y2) = "\n<line x1"&show x1 ++ " y1"&show y1 ++ 
				          " x2"&show x2 ++ " y2"&show y2 ++ 
					  " stroke"&("rgb"++show (r,g,b)) ++
					  " stroke-width"&"1.5" ++ "/>"

polyline :: Bool -> RGB -> Path -> String
polyline smooth (RGB r g b) ps = if null ps then ""
			         else "\n<polyline points"&
				      concatMap (f . round2) qs ++
				      " stroke"&("rgb"++show (r,g,b)) ++
				      " stroke-width"&"1.5" ++ 
				      " fill"&"none" ++ "/>"
		                 where f (x,y) = ' ':show x++',':show y
				       qs = if smooth then spline ps else ps

polygon :: Bool -> RGB -> Path -> String
polygon smooth (RGB r g b) ps = if null ps then ""
		                else "\n<polygon points"&
				     concatMap (f . round2) qs ++ 
				     " fill"&("rgb"++show (r,g,b)) ++ "/>"
		                where f (x,y) = ' ':show x++',':show y
			              qs = if smooth then spline ps else ps

text :: RGB -> Point -> String -> String
text (RGB r g b) (x,y) a = "\n<text x"&show x ++ " y"&show y ++ 
			   " font-family"&"courier" ++ " font-size"&"16" ++ 
			   " fill"&("rgb"++show (r,g,b)) ++ '>':concatMap f a ++ 
			   "</text>" where f '<' = "&lt;"; f '>' = "&gt;"
			   		   f c = [c]

-- SAMPLE PICTURES

drawPiles :: [Int] -> IO ()					   
drawPiles = drawCI . piles red 31121	            		     

drawCastles :: [[Int]] -> IO ()					   
drawCastles = drawCS "piles" . map (piles red 31131)	            		     

fp1 = drawLGraphI "fahrplan" 4					   -- hor=4
								   -- ver=14
								   
orbits = combine $ concat $ take 5 $ iterate (map $ scale 0.5)     -- hor=ver=33
	           [f 8 4,turn 45 $ f 8 4,f 4 8,turn 45 $ f 4 8]       
         where f = elli blue 12111
         
orbitsM = morphing 1 10 [orbits,scale 0.3 orbits]

hilb1  = hilb red 12111 6
hilb2  = hilb red 21111 4					   -- hor=ver=24
hilb3  = hilb red 31111 4		
hilb4  = hilb red 31121 4		
hilb5  = hilb red 14211 4		
hilb6  = hilb red 13111 4		
hilb7  = hilb red 42111 4

hilb8  = hilb red 22121 4					   -- hor=ver=33
hilb9  = hilb red 12112 4		
hilb10 = hilb red 12113 4		
hilb11 = hilb red 12114 4		
hilb12 = flipV $ toCenter hilb1
hilb13 = turn 22 $ hilb1
hilb14 = overlay [hilb1,hilb2]

cant1 = cant red 12111 33
cant2 = flipV $ toCenter cant1
cant3 = overlay [cant1,cant2]
cant4 = cant red 12121 33
cant5 = cant red 13111 33
cant6 = cant red 13113 33
cant7 = cant red 13114 33

snake1 = snake red 12111 33
snake2 = transpose snake1
snake3 = overlay [snake1,snake2]
snake4 = snake red 12211 33

cs1 = overlay [cant3,snake1]					   -- hor=ver=15
cs2 = overlay [cant1,snake3]
cs3 = overlay [cant3,snake3] 					
cs4 = updMod 12121 cs3 		

-- see http://www.win.tue.nl/~hzantema/strfib.html

fibo1 = flipH $ toCenter $ fibo10 21111 			   -- hor=ver=88
fibo2 = flipH $ toCenter $ fibo10 31111
fibo3 = flipH $ toCenter $ fibo10 12111
fibo4 = flipH $ toCenter $ fibo10 14111
fibo5 = flipH $ toCenter $ fibo10 14211
fibo6 = flipH $ toCenter $ fibo10 15111
fibo7 = flipH $ toCenter $ fibo10 13124
fibo8 = flipH $ toCenter $ turn 72 $ fibo red 13124 500 144 72  
fibo9 = turn 225 $ fibo yellow 12111 4150 18 162
fibo10 m = turn 72 $ dropC 100 $ fibo red m 500 144 72
fibo11 m = fibo red m 2000 30 150
fibo12 m = fibo red m 2000 90 120
fibo13 m = fibo red m 2000 20 160
fibo14 m = fibo red m 5000 174 174

cf1 = overlay [cant3,scale 5 $ fibo3]				   -- hor=ver=18
cf2 = overlay [cant5,flipV $ toCenter cant5,scale 5 fibo7]
cf3 = overlay [cant5,flipV $ toCenter cant6,scale 5 fibo7]
cf4 = overlay [cant6,flipV $ toCenter cant6,scale 5 fibo7] 	

phyllo1 = phyllo red 21111 300					   -- hor=ver=14
phyllo2 = phyllo red 41111 300	
phyllo3 = phyllo yellow 21111 600
phyllo4 = phyllo yellow 21113 600
phyllo5 = phyllo yellow 21114 600
phyllo6 = phyllo yellow 21124 600
phyllo7 = phyllo yellow 21134 600

blosR col = blosCurve col 6 rhombH

gras1 = hueCol 1 $ gras red 14111 8				   -- hor=ver=1
gras2 = grasL red 14111 7
gras3 = hueCol 1 $ updCol red $ grasC red 14111 5 $ rhombH green   
gras4 = hueCol 1 $ updCol red $ grasC red 14111 5 $ blosR green
gras5 = hueCol 1 $ grasC red 14111 5 $ blosR green
gras6 = hueCol 1 $ grasC red 14111 5 $ hueCol 1 $ blosR green
gras7 = grasC green 14111 5 $ blosR red

fern1 = hueCol 1 $ fern red 14111 8				   -- hor=ver=4
fern2 = fernL red 14111 8
fern3 = hueCol 1 $ fernL red 14111 8	

fern4 = hueCol 1 $ fernU red 14111 11				   -- hor=ver=3
fern5 = fernUL red 14111 11					   
fern6 = hueCol 1 $ fernUL red 14111 11
fern7 = fernUL red 12111 11 
fern8 = hueCol 1 $ fernU red 12111 12
fern9 = hueCol 1 $ fernU red 12111 14

bush1 = hueCol 1 $ bush red 14111 5				   -- hor=ver=2
bush2 = bushL red 14111 5				           

dragon1 = dragon red 12111 11					   -- hor_ver=1
dragon2 = dragonF red 12111 3333				   -- hor_ver=2
dragon3 = dragonF red 13213 3333

sp1 = combine [snow1 4 12111,scale 0.7 phyllo1] 		   -- hor=ver=12
sp2 = shiftCol 500 sp1			
sp3 = shiftCol 1000 sp1			

circ1 = circ red 13111 33
circ2 = circ red 15111 33
rect1 = rect red 15211 (45,5) 
rect2 = rect red 13211 (5,45)

rain1     = rainbow 1 33 circ1
rain2     = rainbow 1 33 circ2
rain3     = rainbow 1 33 rect1
rain4     = rainbow 1 33 rect2
rain5 m n = rainbow m n $ tria red 13111 33
rain6 m n = rainbow m n $ tria red 13113 33

rads1 = [1..9]++reverse [2..8]
rads2 = [2,2,3,3,4,4,5,5,4,4,3,3]

arc1 = arc red 12111 10 [33,44] 5				   -- hor=ver=4
arc2 = arc red 12111 10 [33,44] 11 				
arc3 = arc red 12111 10 [33,44] 19 				

poly1 = poly red 12111 11 rads1					   -- hor=ver=33
poly2 = poly red 13111 11 rads1
poly3 = poly red 14111 11 rads1
poly4 = poly red 14211 11 rads1
poly5 = poly red 13121 11 rads2
poly6 = combine [poly2,turn (-1.5) poly5] 

poly7 = poly red 15211 30 [40]
poly8 = poly red 15211 5 [5,40]
poly11 = morphing 1 44 [poly7,poly8]
poly12 = turn 36 $ scale 0.5 poly8
poly13 = poly red 12111 2 [0.1]
poly14 m = turn 54 $ poly cyan m 12 [210,70]

leaf1 = leaf red green 15111 111 1000
leaf2 = leaf red green 15111 111 500
leaf3 = leaf red green 15111 111 200

blos1 = blosLeaf red 15111 True 6 [200,150]			   -- hor=ver=3
blos2 = blosLeaf red 15111 True 6 [100,0,0,80,0,60,0,80,0,0]
blos3 = blosLeaf red 15111 True 6 [200,0]
blos4 = blosLeaf red 15111 False 6 [200,0]

cp1 = combine [cant3,scale 0.15 poly1]
cp2 = combine [updMod 12121 $ cant3,scale 0.15 poly1]
cp3 = combine [updMod 12121 $ cant3,scale 0.25 poly6] 		   -- hor=ver=18
cp4 = overlay [cant4,flipV $ toCenter cant4,scale 0.25 poly6]
cp5 = combine [f blue,flipH $ f yellow]
      where f col = hueCol 1 $ updCol col $ bunch 4

grow1 mode = scale 0.5 $ grow tru id [mt, pytree id cyan mode 4, mt,
				      grow hex id [mt,pytree id magenta 6 mode]]
             where mt = emptyC ""
	           tru = single "" green mode trunk
		   hex = single "" blue mode hexa
		   
grow2 mode = grow (inCenter (shine 1 33) quad) id
                  [inCenter (shine (-1) 33) pent, 
		   rain $ base False $ poly red mode 7 [33],
		   pytree rain yellow mode 3, rain hex]
            where quad = single "" yellow mode [p0,(22,-55),(80,-55),(70,22),p0]
	          pent = single "" green mode penta
		  hex  = single "" blue mode hexa
		  rain = inCenter $ rainbow 1 33
	   
grow3 = grow7 id id id 
	   
grow4 = grow7 (rainbow 1 5) id id 
	   
grow5 = grow7 (shine 1 5) (shine 1 11) id
	   
grow6 = grow7 (shine 1 5) (shine 1 11) $ rainbow 1 5

grow7 f g h mode = inCenter f $ overlay [grow1 False,grow1 True]
           where grow1 inner = grow (base inner $ poly yellow mode 16 [111]) id
	        		    $ concat $ replicate 8 [emptyC "",grow2]
                 grow2 = grow (inCenter g $ single "" blue mode cactus) id
		              $ replicate 5 $ shift (45,0) $ inCenter h 
			      $ elli green mode 22 44
					    
grow8 m n = grow c id [single "" blue n hexa,
		       grow (single "" red n square) id [emptyC "",c],
		       base False $ poly red n 5 [111],c,c]
            where c = base False $ snow 1 m 1 4 5 $ turn 54 $ poly red n 5 [222]

morph2 n    = turn 18 $ morphing 1 n [poly8,poly12,turn 72 poly8]
morph3 n    = updMod 23121 $ morph2 n
morph4 n    = morphing 1 n [poly7,poly13]
morph5 n    = morphing 1 n [turn 45 $ poly red 12111 4 [55],tria red 12111 33]
morph6 n    = morphing 1 n [cant3,scale 0.15 poly1]
morph8 n    = morphing 1 n [rainbow 1 33 rect1,rainbow 1 33 rect2]
morph9 m n  = morphing m n $ map (rainbow 1 33 . rect yellow 15211)
			         [(5,45),(45,5)]
morph10 m n = morphing m n $ map (rainbow 1 33 . rect cyan 15214) 
			         [(50,30),(0,30),(50,0)]
morph11 m n = morphing m n $ map (rainbow 1 33 . rect cyan 15211) 
			         [(50,25),(1,22),(50,5)]
morph12 m n = morphing m n $ map ($ poly red 13111 12 [2,12]) [id,shift (50,0)]
morph13 m n = rainbow m n circ1 	   -- morphing m n [circ1,scale 0 circ1]
morph14 m n = rainbow m n circ2 	   -- morphing m n [circ2,scale 0 circ2]
morph15 m n = morphing m n [circ1,shift (15,15) $ scale 0 circ1]	
morph16 m n = morphing m n [circ2,shift (15,15) $ scale 0 circ2]
morph17 m n = morphing m n $ map ($ snow1 6 15111) [id,shift (10,0) . scale 0]
morph18 m n = rainbow m n $ snow1 6 15111  -- combine $ snows1 m n
morph19 n   = morphing 1 n $ map ($ snow1 6 15111) [hscale 0.5,vscale 0.5]
morph20 n   = morphing 1 n $ map ($ snow1 6 15111) [hscale 0.5,id,vscale 0.5]
morph21 n   = morphing 1 n [poly red 13111 50 [44],poly red 13111 5 [1,44]]
morph22 n   = morphing 1 n [rect red 13111 (33,33),tria red 13111 40]
morph23 n   = morphing 1 n [turn 30 $ f 3,turn 45 $ f 4,turn 18 $ f 5,f 6] 
	      where f k = poly red 13111 k [40]
morph24 n   = morphing 1 n [turn 45 $ poly red 13111 4 [55],tria red 13111 33]
morph25 n   = morphing 1 n [snow33 1 6 15111,snow33 0.2 6 15111]
morph26 n   = morphing 1 n [snow34 1 6 15111,snow34 0.2 6 15111]

polys m n x y = [poly red m n [x,y],poly red m n [y,x]]

morph27 n   = morphing 1 n $ polys 15111 5 55 0.1
morph28 n   = morphing 1 n $ map ($ snow1 6 15111) [id,hscale 0.2,id,vscale 0.2]
morph30 n   = morphing 1 n $ polys 13121 5 55 0.1
morph31 n   = morphing 1 n $ polys 14121 5 55 0.1
morph32 n   = morphing 1 n $ polys 13123 5 55 0.1
morph33 n   = morphing 1 n $ polys 13124 5 55 0.1
morph34 n   = morphing 1 n $ polys 13133 5 55 0.1
morph35 n   = morphing 1 n $ polys 13114 5 55 0.1
morph36 n   = morphing 2 n [poly yellow 15111 5 [55,0.5],
 			    poly red 15111 5 [0.5,55]]
morph37 n   = morphing 3 n [poly yellow 15111 5 [55,0.5],
			    poly red 15111 5 [0.5,55]]
morph38 n   = morphing 1 n [snake1,turn 90 snake1]
morph39 n   = morphing 1 n [cant1,turn 90 cant1]
morph40 n   = morphing 1 n $ polys 15111 11 55 0.1
morph41 n   = morphing 2 n $ polys 15111 11 55 0.1
morph42 n   = morphing 1 n $ polys 15211 11 60 0.1 ++ polys 15211 11 40 0.1 ++
			     polys 15211 11 20 0.1
morph43 n   = morphing 1 n $ polys 15111 11 60 0.1 ++ polys 15111 11 40 0.1 ++
			     polys 15111 11 20 0.1

tria1 n = tria red n 222	
tria2 n = tria red n $ -222
rect3 n = rect red n (222,222)

snow1 m    = snow 1 m 1 5 6 . tria1				-- !!
snow2 m    = snow 1 m 1 5 6 . tria2
snow3 m    = shiftCol 111 . snow1 m
snow4 m n  = snow 1 m 1 4 6 $ circ cyan n 222
snow8 m    = snow 1 m 1 5 6 . updCol yellow . tria1
snow9 m n  = snow 1 m 1 4 6 $ poly red n 6 [210,70]
snow10 m n = snow 1 m 1 4 4 $ poly red n 4 [210,70]
snow11 m n = snow 1 m 1 4 4 $ poly red n 8 [210,70]
snow12 m n = snow 1 m 1 4 8 $ poly red n 4 [210,70]
snow13 m   = snow 1 m 1 4 5 . poly14
snow14 m   = snow 1 m 1 4 10 . poly14
snow15 m   = snow 1 m 1 4 15 . poly14
snow16 m   = snow 1 m 1 4 4 . rect3 
snow17 m   = snow 1 m 1 3 8 . rect3 
snow18 m   = snow 1 m 1 5 6 . rect3 				-- !!
snow19 m   = snow 1 m 1 6 6 . rect3 
snow20 m   = snow 1 m 1 5 5 . poly14 
snow21 m   = snow 1 m 1 5 4 . rect3 
snow22 m   = snow 1 m 1 4 6 . tria2 
snow23 m n = snow 1 m 1 5 6 $ combine [tria1 n,tria2 n]  	-- !!
snow24 m n = snow 1 m 1 4 6 $ circ red n 222
snow26 k m = hueCol k . updCol yellow . snow 1 m 1 4 10 . poly14
snow27 m n = snow 1 m 1 4 6 $ poly red n 4 [210]
snow28 m n = snow 1 m 1 4 6 $ poly red n 4 [210,70]
snow29 m n = snow 1 m 1 5 6 $ poly red n 4 [210,70]
snow30 m n = snow 1 m 1 4 6 $ poly red n 4 [210,105]
snow31     = snow33 $ 1/3
snow32     = snow34 $ 1/3
snow33 r m n = snow 1 m 1 4 6 $ poly red n 6 [222,1]
snow34 r m n = snow 1 m 1 4 5 $ turn 18 $ poly red n 5 [222,1]
snow35 m n = snow 1 m 1 5 5 $ turn 54 $ poly red n 5 [333]
snow36 m n = snow 1 m 1 4 5 $ toCenter $ pytree id red n 3
snow37 m n = snow 1 m 1 4 4 $ toCenter $ pytree id red n 3
snow38 m = snow 1 m 1 3 5 . toCenter . grow3
snow39 m = snow 1 m 1 5 6 . tria1				-- !!
snow40 m = snow 1 m 1 5 6 . rect3 				-- !!
snow41   = snow 1 5 (-0.8) 5 8 . rect3
snow42   = snow 1 1 (-1)   5 8 . rect3
snow43   = snow 1 2 0.17   5 6 . rect3
snow44 n = snow 4 2 1 5 6 $ combine $ map (tria blue n) [222,-222]
snow45 n = snow 4 2 1 5 6 $ combine $ map (tria yellow n) [222,-222]

blos7 m = blosLeaf red m True 6 [200,155]

snow46 = snow 1 6 1 2 6       . blos7
snow47 = snow 1 6 (-0.15) 2 6 . blos7
snow48 = snow 1 6 (-0.15) 3 6 . blos7

snow49 = snow 1 4 (-1) 4 6 $ circ red 13113 222
snow50 = snow 1 4 0 4 6 $ circ red 13113 222
snow51 = snow 1 4 1 4 6 $ circ red 13113 222			-- !!
snow52 = snow 4 4 1 4 6 $ circ red 13113 222
snow53 = snow 1 6 1 5 6 $ tria1 15111
snow54 = snow 4 6 1 5 6 $ tria1 15111
snow55 = snow 1 6 1 4 6 $ rect red 15111 (222,222)		-- !!
snow56 = snow 1 6 1 4 6 $ combine $ map (tria red 13113) [222,-222]

snows1 m n = morphing m n $ map ($ snow1 6 15111) [id,scale 0]

snows k = drawCSI ("snows"++show k) $
          case k of 2 -> rains snow1
	  	    3 -> rains snow27
	            4 -> rains snow30
	            5 -> rains snow28
	            6 -> rains snow31
	            7 -> rains snow32
	            8 -> rains snow23
	            9 -> rains snow22
	            11 -> concatMap rainsnow [0.8,0.6,0.4,0.2]
		    _ -> []
	      
rains c  = map morph [3..11] where morph n = rainbow 1 n $ c 6 15111
rainsnow = rains . snow33

morphs  = movie "Pix/morphs" False
snows2  = movieSelect "Pix/snows2" $ cycle9 0
snows3  = movieSelect "Pix/snows11" $ concatMap cycle9 [0,9,18,27,18,9]
snows10 = movies "Pix/snows10" False (map f [2..9]) [(i,j) | i <- [0..7], 
						         j <- cycle9 0]
          where f i = "Pix/snows"++show i
urtiere = movies "Pix/urtiere" False ["morph27","morph27A","urtiere"] $ 
                 [(i,j) | i <- [0,1], j <- [0..22]] ++ [(2,i) | i <- [0..9]]

cycle9 n = [n..n+8]++[n+7,n+6..n+1]

path1 = kpath red 12111 6 1 1 
path2 = kpath red 14211 8 1 1 
path3 = kpath red 14211 8 8 8 
path4 = overlay [path1,flipH $ toCenter path1,path2,flipH $ toCenter path2]
path5 = kpath red 12111 20 1 1 
path6 = kpath red 14211 40 1 1 
path7 = kpath red 12113 40 40 40 

{- kpath 6 1 1 -->
   Just [(1,1), (2,3), (1,5), (3,6), (5,5), (6,3), (5,1), (3,2), (1,3), (2,1),
         (4,2), (6,1), (5,3), (6,5), (4,6), (3,4), (2,6), (1,4), (2,2), (4,1),
	 (6,2), (5,4), (6,6), (4,5), (3,3), (2,5), (4,4), (5,6), (6,4), (5,2),
	 (3,1), (1,2), (2,4), (1,6), (3,5), (4,3)] -}

tour1 = ktour red 12211 8 1 1 					-- hor=ver=44
tour2 = ktour red 14211 8 8 8 
tour3 = overlay [tour1,flipH $ toCenter tour1,tour2,flipH $ toCenter tour2]
tour4 = ktour red 12111 16 8 8 
tour5 = ktour red 12111 18 9 9 					-- hor=ver=22
tour6 = ktour red 12113 18 9 9 
tour7 = ktour red 12114 18 9 9 
tour8 = toCenter $ ktour red 13111 18 9 9 
tour9 = hueCol 1 $ overlay [p,flipH $ toCenter p,q,flipH $ toCenter q]
        where p = ktour red 14211 8 1 1
	      q = ktour red 14211 8 8 8 

{- ktour 8 1 1 -->
   Just [(1,1), (2,3), (3,1), (1,2), (2,4), (1,6), (2,8), (4,7), (6,8), (8,7),
         (7,5), (8,3), (7,1), (5,2), (7,3), (8,1), (6,2), (4,1), (2,2), (1,4), 
	 (2,6), (1,8), (3,7), (5,8), (7,7), (8,5), (6,6), (7,8), (8,6), (7,4), 
	 (8,2), (6,1), (4,2), (2,1), (3,3), (5,4), (3,5), (4,3), (5,1), (6,3),
	 (8,4), (7,2), (6,4), (5,6), (4,8), (2,7), (1,5), (3,6), (1,7), (3,8), 
	 (5,7), (4,5), (5,3), (6,5), (4,6), (6,7), (8,8), (7,6), (5,5), (3,4), 
	 (1,3), (2,5), (4,4), (3,2), (1,1)] -}
