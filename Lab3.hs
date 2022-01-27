import Data.Char

--1)Sa se scrie o functie nrVocale care pentru o lista de siruri de caractere, calculeaza numarul
-- total de vocale ce apar în cuvintele palindrom. Pentru a verifica daca un sir e palindrom, puteti 
-- folosi functia reverse, iar pentru a cauta un element într-o lista puteti folosi functia
-- elem. Puteti defini oricâte functii auxiliare.
-- nrVocale ["sos", "civic", "palton", "desen", "aerisirea"] = 9
numberVocals :: [Char] -> Int
numberVocals [] = 0
numberVocals (x : xs)
    | elem x "aeiouAEIOU"   = 1 + numberVocals xs
    | otherwise = numberVocals xs

nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (x:xs)
    | x == reverse x  = numberVocals x + nrVocale xs
    | otherwise = nrVocale xs

-- 2) Sa se scrie o functie care primeste ca parametru un numar si o lista de întregi, si adauga
-- elementul dat dupa fiecare element par din lista. Sa se scrie si prototipul functiei.
-- f 3 [1,2,3,4,5,6] = [1,2,3,3,4,3,5,6,3]
f :: Int -> [Int] -> [Int]
f _ [] = []
f n (x:xs)
    | odd x  = x : f n xs
    | otherwise = x : n : f n xs 

-- comprehensiune sau selectie
-- 3) Sa se scrie o functie care are ca parametru un numar întreg si determina lista de divizori ai
-- acestui numar. Sa se scrie si prototipul functiei.
-- divizori 4 = [1,2,4]
divizori :: Int -> [Int]
divizori n = [x | x <- [1..n],  n `mod` x == 0]

-- 4) Sa se scrie o functie care are ca parametru o lista de numere întregi si calculeaza lista 
-- listelor de divizori.
-- listadiv [1,4,6,8] = [[1],[1,2,4],[1,2,3,6],[1,2,4,8]]
listadiv :: [Int] -> [[Int]]
listadiv ls = [ divizori x | x <- ls]

-- 5) Scrieti o functie care date fiind limita inferioara si cea superioara (întregi) a unui interval
-- închis si o lista de numere întregi, calculeaza lista numerelor din lista care apartin intervalului.
-- De exemplu:
-- inInterval 5 10 [1..15] == [5,6,7,8,9,10]
-- inInterval 5 10 [1,3,5,2,8,-1] = [5,8]
-- a) Folositi doar recursie. Denumiti functia inIntervalRec
inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec _ _ [] = []
inIntervalRec inf sup (x:xs)
    | inf <= x && x<= sup  = x: inIntervalRec inf sup xs
    | otherwise  = inIntervalRec inf sup xs

-- b) Folositi descrieri de liste. Denumiti functia inIntervalComp  (comprehensiune)
inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp inf sup xs = [x | x <- xs, inf <= x && x <= sup]

-- 6) Scrieti o functie care numara câte numere strict pozitive sunt într-o lista data ca argument.
-- De exemplu:
-- -- pozitive [0,1,-3,-2,8,-1,6] == 3
-- a) Folositi doar recursie. Denumiti functia pozitiveRec
pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (x:xs)
    | x > 0     = 1 + pozitiveRec xs
    | otherwise = pozitiveRec xs

-- b) Folositi descrieri de liste. Denumiti functia pozitiveComp.
-- • Nu puteti folosi recursie, dar veti avea nevoie de o functie de agregare. (Consultati
-- modulul Data.List ). De ce nu e posibil sa scriem pozitiveComp doar folosind descrieri
-- de liste?
pozitiveComp :: [Int] -> Int
pozitiveComp xs = length [x | x <- xs, x > 0]

-- 7) Scrieti o functie care data fiind o lista de numere calculeaza lista pozitiilor elementelor 
-- impare din lista originala. 
-- De exemplu:
-- -- pozitiiImpare [0,1,-3,-2,8,-1,6,1] == [1,2,5,7]
-- a) Folositi doar recursie. Denumiti functia pozitiiImpareRec.
-- • Indicatie: folositi o functie ajutatoare, cu un argument în plus reprezentând pozitia
-- curenta din lista.
pozitiiImpareCuIndex :: [Int] -> Int -> [Int]
pozitiiImpareCuIndex [] _ = []
pozitiiImpareCuIndex (x:xs) n 
    | odd x    = n : pozitiiImpareCuIndex xs n+1
    | otherwise = pozitiiImpareCuIndex xs n+1  
pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec xs = pozitiiImpareCuIndex xs 0

-- b) Folositi descrieri de liste. Denumiti functia pozitiiImpareComp.
-- • Indicatie: folositi functia zip pentru a asocia pozitii elementelor listei (puteti cauta
-- exemplu în curs).
pozitiiImpareComp  :: [Int] -> [Int]
pozitiiImpareComp xs = [ ind | (ind, x) <- [0..] `zip` xs, odd x]

-- 8) Scrieti o functie care calculeaza produsul tuturor cifrelor care apar în sirul de caractere 
-- dat ca intrare. Daca nu sunt cifre în sir, raspunsul functiei trebuie sa fie 1 . 
-- De exemplu:
-- -- multDigits "The time is 4:25" == 40
-- -- multDigits "No digits here!" == 1
-- a) Folositi doar recursie. Denumiti functia multDigitsRec
multDigitsRec :: [Char] -> Int
multDigitsRec [] = 1
multDigitsRec (x : xs)
    | isDigit x  = digitToInt x * multDigitsRec xs
    | otherwise  = multDigitsRec xs
 
-- b) Folositi descrieri de liste. Denumiti functia multDigitsComp
-- • Indicatie: Veti avea nevoie de functia isDigit care verifica daca un caracter e cifra si functia
-- digitToInt care transforma un caracter in cifra. Cele 2 functii se afla în pachetul Data.Char.

multDigitsComp :: [Char] -> Int
multDigitsComp xs = product[x | x <- xs, isDigit x]  

