import Distribution.Simple.Utils (xargs)
--import Main (fizzbuzz1)
--1
--poly2 :: Double-> Double->Double->Double->Double
--poly2 a b c x = a*x^2 + b*x + c

--2
eeny :: Integer -> String
eeny x 
     |even x   = "eeny"
     |otherwise  ="meeny"

--3 
fizzbuzz1 :: Integer -> String
fizzbuzz1 x 
        | x `mod` 15 == 0  ="FizzBuzz"
        | x `mod` 3 == 0  = "Fizz"
        | x `mod` 5 == 0  = "Buzz"
        |otherwise  = ""

fizzbuzz2 :: Integer -> String
fizzbuzz2 x 
        | x `mod` 15 == 0  ="FizzBuzz"
        | x `mod` 3 == 0  = "Fizz"
        | x `mod` 5 == 0  = "Buzz"
        |otherwise  = ""

--4
tribonacci1 :: Integer -> Integer 
tribonacci1 n
        | n==1 =1
        | n==2 =1
        | n==3 =2
        | otherwise = tribonacci1 (n-1) + tribonacci1 (n-2) + tribonacci1 (n-3)

tribonacci2 :: Integer -> Integer 
tribonacci2 1 = 1
tribonacci2 2 = 1
tribonacci2 3 = 2
tribonacci2 n = tribonacci2 (n-1) + tribonacci2 (n-2) + tribonacci2 (n-3)

--5
bimonial :: Integer -> Integer -> Integer 
bimonial 0 k = 0
bimonial n 0 = 1
bimonial n k = bimonial(n-1)k + bimonial (n-1)(k-1)

--6a)
verifL :: [Int] -> Bool 
verifL x
        | (length x) `mod` 2 == 0  =True
        | otherwise =False

--b)
takefinal :: [Int] -> Int -> [Int]
takefinal l n  = drop ((length l)-n)(l)

--c)
remove :: String -> Int -> String 
remove c n = (take (n)(c)) ++ drop((length c)-n)(c)  
        
--7)
--a)
myreplicate :: Int -> Int -> [Int] 
myreplicate 0 _ = []
myreplicate n v
        | n>0        = v:myreplicate(n-1)(v) 
        | otherwise  = []

--b)
sumImp :: [Int] -> Int 
sumImp[]=0
sumImp (h:t)
        |h `mod` 2 > 0 = h+sumImp(t)
        |otherwise     = sumImp(t)  

--c)
totalLen :: [String] -> Int 
totalLen[] = 0
totalLen (sir:lista)
        |(sir!!0) == 'A' = length(sir) + totalLen(lista)
        |otherwise       = totalLen(lista)
