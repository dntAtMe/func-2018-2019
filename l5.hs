-- Kacper Pieniążek, 2366606
--
import Data.List
--
-- 52
noEven xs = foldr (\_ acc -> acc + 1) 0 $ filter even xs 

-- 54
f1 = foldl (+) 0 [1..10000000]
f2 = foldr (+) 0 [1..10000000]
f3 = sum [1..10000000]

-- 55
approx n = foldr (\elem acc -> acc + 1 / foldl (*) 1 [1..elem]) 0 [1..n]

-- 56
calc xs = foldl (\acc elem -> acc + (-1)^(elem+1) * xs!!(elem-1)) 0 [1..length xs]

avg :: (Fractional a) => [a] -> a
avg xs = undefined