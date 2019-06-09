--  Kacper Pieniążek, 236606
--

-- 63
m91 :: Int -> Int
m91 n 
    | n > 100   = n - 10
    | n <= 100  = m91 $ m91 $ n + 11 

m91tab :: [Int] -> [Int]
m91tab ns = map m91 ns

data IntOrString = Mint Int | Mstr String

isInt x = x == fromInteger (round x)

rep :: IntOrString -> String
rep (Mint a) = show a 
rep (Mstr a) = a 

instance Eq IntOrString where
    x == y = rep x == rep y

data BTree a = L a | N a (BTree a) (BTree a) deriving (Show, Eq)