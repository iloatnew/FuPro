--data Konto = Konto Float Kunden deriving (Show)
--data Kunden = Kunden String String String deriving (Show)
-- 这种写法也是正确的, 但题目要求带有attribute名称，所以采用下面的写法

data Kunden = Kunden { vorname, name, adresse :: String} deriving (Show)

data Konto = Konto { kontostand :: Float, besitzer :: Kunden} deriving (Show)


bspkonto = Konto  9999.99  (Kunden "aa" "aa" "ss")
-- ohne attribute ： 也就是按照本来的attribute顺序直接赋值 （9999.99也就是kontostand，等等。。。）

li'skonto = Konto {  besitzer = Kunden { vorname = "wentao",name = "li",adresse = "Essen" }, kontostand = 9999.99 }
-- 如果给出了atrribute，顺序就可以改变 比如这里把stand移到了后边


-- 课堂例子：
data Person = Person String String String
-- 直接定义一种data 不需要括号{}

data Person' = Person' { vorname' :: String, nachname' :: String, adresse' :: String}
-- 带atrribute名称的定义方法
-- atrribute 需要以小写字母开始, 并且attri其实也是一个function.
-- type 要以大写字母开始
