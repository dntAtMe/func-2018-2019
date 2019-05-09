import Data.Numbers.Primes(primes, isPrime)

-- 1
--main = interact wordCount
--    where wordCount input = show $ length $ words input
wordCount input = show $ length $ input

-- 2
phi :: Int -> Int
phi n = length $ filter (== 1) $ map (gcd n) [1..n]

divsum :: Int -> Int
divsum n = sum $ map phi [x | x <- [1..n], n `rem` x == 0]

-- 3
triple :: Int -> [(Int, Int, Int)]
triple x = [(a, b, c) | a <- [1..x], b <- [1..a], c <- [1..b], gcd b c == 1, a^2 == b^2 + c^2]

-- 4
recFib 0 = 0
recFib 1 = 1
recFib n = recFib (n-1) + recFib (n-2)

constFib n = round $ phi ** fromIntegral n / sq5
  where
    sq5 = sqrt 5 :: Double
    phi = (1 + sq5) / 2

fib n = fibs 0 1 !! n
    where
        fibs a b = a:fibs b (a+b) 

-- 5
binom :: Integer -> Integer -> Integer
binom = loop 1 1
  where
    loop rn rd _ 0 = rn `div` rd
    loop _  _  0 _ = 0
    loop rn rd n k = loop (rn * n) (rd * k) (n-1) (k-1)

-- 6
listPerfectNumbers :: Integral a => a -> [a]
listPerfectNumbers n = 
    takeWhile (< n) [2 ^ (x - 1) * (2 ^ x - 1) | x <- primes, isPrime(2 ^ x - 1)]

listPerfectNums :: Integral a => a -> [a]
listPerfectNums n = 
    [val | val <- [1..n], sum [x | x <- [1..div val 2], val `rem` x == 0] == val ]
