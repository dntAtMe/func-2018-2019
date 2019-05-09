import Data.List


-- 1. Konwersja typów: Int, Integer <- -> Float, Double

--fromInteger :: Num a => Integer -> a
--toInteger :: Integral a => a -> Integer

--fromIntegral :: (Num b, Integral a) => a -> b
--realToFrac :: (Real a, Fractional b) => a -> b

--fromRational :: Fractional a => Rational -> a
--toRational :: Real a => a -> Rational

-- 2.
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = (rev xs) ++ [x]

--3.
fib n = fibs!!n

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

--4.
middle :: (Ord a) => a -> a -> a -> a
middle x y z = (\[_,mid,_] -> mid) $ sort $ x:y:z:[]

--5.
qs []     = []
qs (x:xs) = qs [t| t <- xs, t <= x] ++
            [x] ++
            qs [t| t <- xs, t > x] 

qs' []     = []
qs' (x:xs) = qs' lesser ++ [x] ++ qs' greater
    where
        lesser  = filter (< x) xs
        greater = filter (>= x) xs

--6.
inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : (map (x:) (Main.inits xs))

--7.
partitions :: (Eq a) => [a] -> [([a], [a])]
partitions xs = [(ys, xs \\ ys) | ys <- Main.inits xs]

partitions' :: (Eq a) => [a] -> [[ [a] ]]
partitions' [] = [[]]
partitions' xs = [ys : [partitions' (xs \\ ys)] | ys <- Main.inits xs]

            

--8.
nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs) = x : Main.nub (filter (/= x) xs)

--9.
permutations' :: (Eq a) => [a] -> [[a]]
permutations' [] = [[]]
permutations' xs = [e : ys | e <- xs, ys <- permutations' $ delete e xs]

--10.

zeros n  
    | n < 5 = 0
    | otherwise = length [1..n `div` 5] + (zeros $ n `div` 5) 