-- 1. Scrieti o functie generică firstEl care are ca argument o listă de perechi de tip (a,b) si întoarce lista primelor elementelor din fiecare pereche:
-- firstEl [('a',3),('b',2), ('c',1)]  == "abc"

firstEl :: [(a,b)] -> [a]
firstEl = map (\(x,y) -> x)

-- 2. Scrieti functia sumList care are ca argument o listă de liste de valori Int si întoarce lista sumelor elementelor din fiecare listă (suma elementelor 
-- unei liste de întregi se calculează cu functia sum):
-- sumList [[1,3], [2,4,5], [], [1,3,5,6]]  == [4,11,0,15]

sumList :: [[Int]] -> [Int]
sumList = map (foldr (+) 0)


-- 3. Scrieti o functie prel2 care are ca argument o listă de Int si întoarce o listă în care elementele pare sunt înjumătătite, iar cele impare sunt dublate:
-- *Main> prel2 [2,4,5,6]
-- [1,2,10,3]

prel2 :: [Int] -> [Int]
prel2 = map (\x -> if even x then div x 2 else 2*x)


-- 4. Scrieti o functie care primeste ca argument un caracter si o listă de siruri, rezultatul fiind
-- lista sirurilor care contin caracterul respectiv (folositi functia elem).

cautaCaracter :: Char -> [String] -> [String]
cautaCaracter c = filter (\x -> elem c x)


-- 5. Scrieti o functie care primeste ca argument o listă de întregi si întoarce lista pătratelor numerelor impare.

patratImp :: [Int] -> [Int]
patratImp =  map (^2) . filter (odd)
--filter: le iau pe cele impare
--map: le ridic la patrat


-- 6. Scrieti o functie care primeste ca argument o listă de întregi si întoarce lista pătratelor 
-- numerelor din pozitii impare. Pentru a avea acces la pozitia elementelor folositi zip.

patratPozImp :: [Int] -> [Int]
patratPozImp list = map (\(x,y) -> y^2) . filter (\(x,y) -> odd x) $ zip [0..] list   --numaratoarea incepe de la 0
                                                    --filter: le iau pe cele de pe poz impare
                                                    --map: le ridic la patrat


-- 7. Scrieti o functie care primeste ca argument o listă de siruri de caractere
-- si întoarce lista obtinută prin eliminarea consoanelor din fiecare sir.
-- numaiVocale ["laboratorul", "PrgrAmare", "DEclarativa"]  == ["aoaou","Aae","Eaaia"]

numaiVocale :: [String] -> [String]
numaiVocale = map (\x -> filter (\c -> elem c "aeiouAEIOU") x)          


-- 8. Definiti recursiv functiile mymap si myfilter cu aceeasi functionalitate ca si functiile predefinite.

mymap :: (a -> b) -> [a] -> [b]
-- mymap f list = map (\x -> f x) list
mymap f list = [f x | x <- list]

myfilter :: (a -> Bool) -> [a] -> [a]   --f intoarce True/False: lasa in lista doar cele care intorc True
-- myfilter f list = filter (\x -> f x) list
myfilter f list = [x | x <- list, f x ]

-- 9. Calculati suma pătratelor elementelor impare dintr-o listă dată ca parametru.

sumPatratImp :: [Int] -> Int
sumPatratImp = foldr (+) 0 . map (^2) . filter (odd)

-- 10. Scrieti o functie care verifică faptul că toate elementele dintr-o listă sunt True, folosind foldr.

verifTrue :: [Bool] -> Bool
verifTrue = foldr (&&) True

-- 11.
-- (a) Scrieti o functie care elimină un caracter din sir de caractere.
rmChar :: Char -> String -> String
-- rmChar c = filter (\x -> x /= c) 
rmChar c = filter ( /= c) 

-- (b) Scrieti o functie recursivă care elimină toate caracterele din al doilea argument care se găsesc în primul argument, folosind rmChar.
rmCharsRec :: String -> String -> String
rmCharsRec [] a =  a
rmCharsRec (x : xs) ys = rmCharsRec xs (rmChar x ys) 

-- rmCharsRec ['a'..'l'] "fotbal" == "ot"

-- (c) Scrieti o functie echivalentă cu cea de la (b) care foloseste foldr în locul recursiei si rmChar.
rmCharsFold :: String -> String -> String
rmCharsFold xs ys = foldr (\x -> rmChar x) ys xs 