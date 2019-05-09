
import Data.Char

-- 39
mmap :: (a -> b) -> [[a]] -> [[b]]
mmap f = map $ map f 
-- mmap toLower [['a', 'a'], ['b']]

mmap' :: (a -> b) -> [[a]] -> [[b]]
mmap' = map . map

mmmap :: (a -> b) -> [[[a]]] -> [[[b]]]
mmmap f = map $ map $ map f 
-- mmmap toLower [[['a'], ['b', 'b']], [['c', 'c', 'c']]]

mmmap' :: (a -> b) -> [[[a]]] -> [[[b]]]
mmmap' = map . map . map 


-- 40
plus :: (Num a) => a -> a -> a
plus = (\x y -> x + y)

mul :: (Num a) => a -> a -> a
mul = (\x y -> x * y)
 
-- 41 currying first arg

ff :: (Integral a) => a -> a 
ff = ( 2 ^ ) 

gg :: (Integral a) => a -> a
gg = ( ^ 2 )

-- 43
t x y = (\x -> (\y -> x + 2 * y)) (x * y)
-- po alfa transformacji i beta redukcji:
t' x y = (\z -> x * y + 2 * z)

-- 44 uproszczenie h to h'
f = \x -> x * x
g = \y -> f (f y)
h = g . g

h' = (^ 16)  

-- 45 uprość wyrażenie com
--com = (\x -> (x x))(\x -> x)

-- 46
-- (head $ map (\x y -> (x * x) + (y * y) ) [2,3,4]) 5

-- 48
span' :: (a -> Bool) -> [a] -> ([a], [a])
span' _ [] = ([], [])
span' p xs@(x:rest)
    | p x       = let (ys, zs) = span' p rest in (x:ys, zs)
    | otherwise = ([], xs)

ecd :: (Eq a) => [a] -> [a]
ecd [] = []
ecd xs@(x:xs') = x : helper xs x
    where
        helper [] y = []
        helper xs@(x:xs') y 
            | x == y    = helper xs' y 
            | otherwise = x : helper xs x

pack :: (Eq a) => [a] -> [[a]]
pack ys@(y:ys') = helper [y] ys' []
        where
            helper current [] acc = (current : acc)
            helper current@(c:_) ys@(y:ys') acc 
                | c == y    = helper (y:current) ys' acc
                | otherwise = current : helper [y] ys' (acc)
           
pack' :: (Eq a) => [a] -> [[a]]
pack' [] = []
pack' ys@(y:ys') = take n ( repeat y) : pack' (drop n ys)
            where n = length $ takeWhile ( == y) ys 

rleEncode :: String -> [(Int, Char)]
rleEncode string = map (\xs@(x:xs') -> (length xs, x)) $ pack string

rleDecode :: [(Int, Char)] -> String
rleDecode code = concatMap (\(n, c) -> take n (repeat c)) code