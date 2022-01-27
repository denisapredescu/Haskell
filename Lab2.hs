-- 1. Să se scrie o functie poly2 care are patru argumente de tip Double, a,b,c,x si 
-- calculează a*xˆ2+b*x+c. Scrieti si signatura functiei (poly :: ceva).
-- 2. Să se scrie o functie eeny care întoarce “eeny” pentru input par si 
-- “meeny” pentru input impar. Hint: puteti folosi functia even 
-- (puteti căuta pe https://hoogle.haskell.org/).

poly2 :: Double -> Double -> Double -> Double -> Double
poly2 a b c x = a * x^2 + b*x + c

eeny :: Int -> [Char]
eeny x 
    | rem x 2 == 0  = "eeny"
    | otherwise     = "meeny"

-- 3. Să se scrie o functie fizzbuzz care întoarce “Fizz” pentru numerele 
-- divizibile cu 3, “Buzz” pentru numerele divizibile cu 5 si “FizzBuzz” pentru 
-- numerele divizibile cu ambele. Pentru orice alt număr se întoarce sirul vid.
-- Pentru a calcula modulo a două numere puteti folosi functia mod. Să se scrie 
-- această functie în 2 moduri: folosind if si folosind gărzi (conditii).

fizzbuzz1 :: Int -> [Char]
fizzbuzz1 x
    | mod x 15 == 0  = "FizzBuzz"
    | mod x 3 == 0  = "Fizz"
    | mod x 5 == 0  = "Buzz"
    | otherwise     = ""

fizzbuzz2 :: Int -> [Char]
fizzbuzz2 x = 
    if mod x 15 == 0 then "FizzBuzz"
    else
        if mod x 3 == 0  then "Fizz"
        else
            if mod x 5 == 0  then "Buzz"
            else ""


--4 
cazuri :: Int -> Int
cazuri n
    | n == 1 = 1
    | n == 2 = 1
    | n == 3 = 2
    | otherwise = cazuri(n-1) + cazuri(n-2) + cazuri(n-3)  

ecuational :: Int -> Int
ecuational 1 = 1
ecuational 2 = 1
ecuational 3 = 2
ecuational n = ecuational(n-1) + ecuational(n-2) + ecuational(n-3) 

-- 5
-- B(n,k) = B(n-1,k) + B(n-1,k-1)
-- B(n,0) = 1
-- B(0,k) = 0
binomial :: Integer -> Integer -> Integer
binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial (n-1)k + binomial (n-1)(k-1)


-- 6. 
-- a) verifL - verifică dacă lungimea unei liste date ca parametru este pară
verifL :: [Int] -> Bool
verifL xs = if length xs `mod` 2 == 0 then True else False

-- b) takefinal - pentru o listă dată ca parametru si un număr n, întoarce lista 
-- cu ultimele n elemente.  Dacă lista are mai putin de n elemente, se intoarce 
-- lista nemodificată.
takefinal :: [Int] -> Int -> [Int]
-- varianta 1
-- takefinal xs n 
--     | length xs <= n = xs
--     | otherwise      = takefinal (tail(xs)) n

--varianta 2
takefinal xs n = drop (length xs - n) xs

--puteam sa fac cu drop => drop n lista => sterge primele n numele din lista
    
-- Cum trebuie să modificăm prototipul funct, iei pentru a putea fi folosită s, i 
-- pentru s, iruri de caractere?
-- takefinal :: [Char] -> Int -> [Char]


-- c) remove - pentru o listă si un număr n se întoarce lista din care se sterge
-- elementul de pe pozitia n.
-- (Hint: puteti folosi functiile take si drop). Scriti si prototipul functiei.
remove :: [Int] -> Int -> [Int]
remove xs n = take n xs ++ drop (n+1) xs


-- 7. Exercitii: să se scrie urmatoarele functii folosind recursivitate:
-- a) myreplicate - pentru un întreg n si o valoare v întoarce lista de lungime n 
-- ce are doar elemente egale cu v. Să se scrie si prototipul functiei.
myreplicate :: Int -> Int -> [Int]
myreplicate n v 
    | n == 0    = []
    | otherwise = v : myreplicate (n-1) v

-- b) sumImp - pentru o listă de numere întregi, calculează suma valorilor impare.
-- Să se scrie si prototipul functiei.
sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (x:xs)
    | odd x     = x + sumImp xs
    | otherwise = sumImp xs

-- c) totalLen - pentru o listă de siruri de caractere, calculează suma lungimilor 
-- sirurilor care încep cu caracterul ‘A’.
totalLen :: [String] -> Int
totalLen [] = 0
totalLen (x : xs) 
    | length x > 0 && head x == 'A' = length x + totalLen xs
    | otherwise  = totalLen xs

