--FuPro Blatt5 Gruppe9 
-- Li Wentao 198341
-- Kurinchinilavan Thangeswaran 156190 
--5-1
solutions :: [(Int, Int, Int)]
--solutions = [ (x,y,z)| zone <- [1..]
--		       , z <- [-zone..zone]
--                     , y <- [-zone..zone]
--                     , x <- [-zone..zone]
--                     , 2*x^3 + 5*y + 2 == z^2
--            ]
solutions = [ (x,y,z)| z <- [0..]
		, x <- [0,z^2]
		, y <- [0,z^2]
		, 2*x^3 + 5*y + 2 == z^2
            ]
--5-2 a)
data Nat    = Zero | Succ Nat deriving Show
data PosNat = One | Succ' PosNat deriving Show
data Int'   = Zero' | Plus PosNat | Minus PosNat deriving Show

drei = Plus (Succ' (Succ' One))

--b)
data Zahlen = Durch Int' PosNat deriving Show

--c)
minusDrei = Minus (Succ' (Succ' One))
zwei = Succ' One
c = Durch minusDrei zwei

--5-3 a)
natLength :: [a] -> Nat
natLength [] = Zero
natLength (a:as) = Succ (natLength as)

--b)
natDrop :: Nat -> [a] -> [a]
natDrop _ [] = []
natDrop Zero a = a
natDrop (Succ nat) (a:as) = natDrop nat as

--c)
data Colist a = Colist {split :: Maybe (a,Colist a)} deriving Show

col1 = Colist (Just (999,Colist (Just ( 123,(Colist Nothing)))))

colistIndex :: Colist a -> Int -> a 
colistIndex ( Colist ( Just (a,_) ) ) 0  = a
--bei "| n>0" definiert man, dass n > 0 sein muss
colistIndex ( Colist ( Just (a,b) ) ) n | n>0 = colistIndex b (n-1) 

--d)
data Stream a = (:<) {hd :: a, tl :: Stream a} deriving Show
--Stream ist immer unendlich
blink,blink' :: Stream Int
blink  = 0:<blink'
blink' = 1:<blink

--"| n>0"
streamTake :: Int -> Stream a -> [a]
streamTake 0 ((:<) a _) = [a]
streamTake n ((:<) a b) | n>0= a:streamTake (n-1) b

--5-4
type ID = Int
data Bank = Bank [(ID,Account)] deriving Show
data Account = Account { balance :: Int, owner :: Client } deriving Show
data Client = Client { name, surname, address:: String } deriving Show

poor = Client "peter" "petri" "poorstreet"
middleclass = Client "marven" "max" "middlestreet"
rich = Client "ricky" "ralf" "richstreet"
acc1 = Account 1000 poor
acc2 = Account 10000 middleclass
acc3 = Account 100000 rich
bspBank = Bank [(1,acc1),(2,acc2),(3,acc3)]

--a)
credit :: Int -> ID -> Bank -> Bank
--credit add i (Bank list) = (Bank (credit' add i list)) where
--	credit' add i [] = []
--	credit' add i ((id,(Account balance c)):next) = 
--		if id==i 
--			then (id,Account (balance+add) c) : (credit' add i next)
--		else (id,Account balance c) : (credit' add i next)
--kann man auch mit map arbeiten:
credit amount i (Bank bank) = Bank $ map f bank
	where f  (i',Account b o) 
		| i' == i = (i',Account (b+amount) o)
		| otherwise = (i',Account b o)


--b)
debit :: Int -> ID -> Bank -> Bank
debit sub i (Bank list) = (Bank (debit' sub i list)) where
	debit' sub i [] = []
	debit' sub i ((id,(Account balance c)):next) = 
		if id==i 
			then (id,Account (balance-sub) c) : (debit' sub i next)
		else (id,Account balance c) : (debit' sub i next)

--c)
transfer :: Int -> ID -> ID -> Bank -> Bank
--transfer num debid creid bank = (debit num debid (credit num creid bank))
--kann man auch bank in beiden Seiten verlassen
transfer num debid creid = debit num debid . credit num creid
