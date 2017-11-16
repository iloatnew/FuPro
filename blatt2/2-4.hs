x + 3 * y * z
--加klammer:
x + (3 * y * z)
x + ((3 * y) * z)
-- haskell中是从左往右计算同级运算
(+) x ((*) ((*) 3 y) z)

(((add3 1) 2) 3)
--不需要转换，已经是了

f $ g . h x
f $ ( g (h x) ) 
($) f ( (.) g (h x) )
