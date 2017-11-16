module Functions where

import Data.Array(Ix,Array,array,(!),bounds,index,range,listArray)
import Data.Maybe
import Data.List(zip5,permutations)
import Data.Monoid (Monoid(mempty,mappend))
import Data.IORef
import Control.Monad
import System.Process
import System.IO.Unsafe
import qualified Data.List as DL
import Data.Set (fromList,toList,findMin)
import qualified Data.Set as DS
import qualified Data.Map.Strict as DMS


indices :: [a] -> [Int]             
indices s = [0..length s-1]

perms :: [a] -> [[a]]

perms [] = [[]]
perms s  = concatMap f $ indices s where
	f i = map (s!!i:) $ perms $ take i s++drop (i+1) s

fromJust :: Maybe a -> a
fromJust (Just a) = a

