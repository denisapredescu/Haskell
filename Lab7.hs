--1.
data Fruct = Mar String Bool | Portocala String Int   --nr felii dintr-o portocala
                                                      --Bool: marul are viermi
                                                      --string: soi 

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [ 
                Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True
             ]

-- a) Scrieti o functie care indică dacă un fruct este o portocală de Sicilia sau nu. Soiurile de portocale din 
-- Sicilia sunt Tarocco, Moro si Sanguinello. De exemplu,
test_ePortocalaDeSicilia1 = ePortocalaDeSicilia (Portocala "Moro" 12) == True
test_ePortocalaDeSicilia2 = ePortocalaDeSicilia (Mar "Ionatan" True) == False

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Mar _ _) = False
ePortocalaDeSicilia (Portocala a b) = a == "Tarocco" || a == "Moro" || a == "Sanguinello"

-- b) Scrieti o functie care calculează numărul total de felii ale portocalelor de Sicilia dintr-o listă de fructe.
test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52

nrFeliiSicilia :: [Fruct] -> Int

-- nrFeliiSicilia [] = 0
-- nrFeliiSicilia (Portocala a b : xs) = if ePortocalaDeSicilia (Portocala a b) then b + nrFeliiSicilia xs else nrFeliiSicilia xs
-- nrFeliiSicilia (_ : xs) = nrFeliiSicilia xs

--saaau
nrFeliiSicilia xs = sum [b | (Portocala a b) <- xs, ePortocalaDeSicilia (Portocala a b)]
                        ---iau din lista doar elementele de tip Portocala care sunt din Sicilia

-- c) Scrieti o functie care calcuelază numărul de mere care au viermi dintr-o lista de fructe.
test_nrMereViermi = nrMereViermi listaFructe == 2 

nrMereViermi :: [Fruct] -> Int
nrMereViermi xs = length [Mar a b | (Mar a b) <- xs, b] 

--Exercitiul 2
type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
              deriving Show
-- a) Scrieti o functie care întoarce "Meow!" pentru pisică si "Woof!" pentru câine.

vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste _ = "Woof!"  

-- b) Vă reamintiti tipul de date predefinit Maybe data Maybe a = Nothing | Just a
-- scrieti o functie care întoarce rasa unui câine dat ca parametru sau Nothing dacă parametrul este o pisică.
rasa :: Animal -> Maybe String
rasa (Pisica _)  = Nothing
rasa (Caine n r) = Just r 

-- Exercitiul 3
-- Se dau urmatoarele tipuri de date ce reprezintă matrici cu linii de lungimi diferite:
data Linie = L [Int]
            deriving Show
data Matrice = M [Linie]
               deriving Show

-- a) Scrieti o functie care verifica daca suma elementelor de pe fiecare linie este egala cu o valoare n. 
-- Rezolvati cerinta folosind foldr.
test_verif1 = verifica (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 10 == False
test_verif2 = verifica (M[L[2,20,3], L[4,21], L[2,3,6,8,6], L[8,5,3,9]]) 25 == True

verifica :: Matrice -> Int -> Bool
-- var 1
-- verifica (M mat) n = and [ (foldr (+) 0 lista) == n | (L lista) <- mat] 
-- am folosit foldr pe fiecare linie din matrice pentru a determina suma elementelor

-- var 2
verifica (M mat) n = foldr (&&) True . map (\(L linie) -> sum linie == n) $ mat
-- dupa ce am calculat sumele si am verificat daca sunt egale cu numarul n dat ca paramentru, aplic foldr pentru a
-- determina daca toate sumele au fost egale cu n

-- b) Scrieti o functie doarPozN care are ca parametru un element de tip Matrice si un numar intreg n, si care verifica 
-- daca toate liniile de lungime n din matrice au numai elemente strict pozitive.

testPoz1 = doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == True
testPoz2 = doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == False

-- doarPozN :: Matrice -> Int -> Bool
-- doarPozN (M mat) n = foldr (&&) True . map numaiPoz . filter (\(L linie) -> length linie == n) $ mat
--                      where numaiPoz xs
--                               | xs == []  = True 
--                               | head xs < 0 = False
--                               | otherwise = numaiPoz (tail xs)
--nu e ok var de mai sus pentru ca numaiPoz primeste o lista nu un L lista


doarPozN :: Matrice -> Int -> Bool
doarPozN (M mat) n = foldr (&&) True . map numaiPoz . filter (\(L linie) -> length linie == n) $ mat
                     where numaiPoz (L linie) = if product linie > 0 
                                                   then True
                                                else False

-- c) Definiti predicatul corect care verifică dacă toate liniile dintr-o matrice au aceeasi lungime.

testcorect1 = corect (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) == False
testcorect2 = corect (M[L[1,2,3], L[4,5,8], L[3,6,8], L[8,5,3]]) == True

corect :: Matrice -> Bool
corect (M mat) = foldr (&&) True [ x == y | (x, y) <- zip (drop 1 (lungimi mat)) (lungimi mat)]
                 where lungimi = map (\(L linie) -> length linie)






