-- 1. Folosind numai metoda prin selectie definiti o functie astfel încât factori n întoarce lista 
-- divizorilor pozitivi ai lui n.
factori :: Int -> [Int]
factori n = [x | x <- [1..n], n `mod` x == 0] 

-- 2. Folosind functia factori, definiti predicatul prim n care întoarce True dacă si numai dacă n 
-- este număr prim.
prim :: Int -> Bool
prim x = length (factori x) == 2 
-- prim n 
--     | length (factori n) == 2  = True
--     | otherwise                = False

-- 3. Folosind numai metoda prin selectie si functiile definite anterior, definiti functia
-- numerePrime = undefined
-- astfel încât numerePrime n întoarce lista numerelor prime din intervalul [2..n].
numerePrime :: Int -> [Int]
numerePrime n = [x | x <- [2..n], prim x]

-- 4. Definiti functia myzip3 care se comportă asemenea lui zip dar are trei argumente:
-- myzip3 [1,2,3] [1,2] [1,2,3,4] == [(1,1,1),(2,2,2)]
myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3 xs ys zs = [(x,y,z) | (a,x) <- zip [1..] xs, 
                             (b,y) <- zip [1..] ys, 
                             (c,z) <- zip [1..] zs, 
                             x == y && x == z]

-- 5. Folosind metoda prin selectie, functia and si functia zip, completati definitia functiei
-- ordonataNat care verifică dacă o listă de valori Int este ordonată, relatia de ordine fiind 
-- cea naturală:
ordonataNat :: [Int] -> Bool
ordonataNat [] = True
ordonataNat [x] = True
ordonataNat (x:xs) = and [a <= b | (a,b) <- zip (x:xs) xs]

-- 6. Folosind doar recursie, definiti functia ordonataNat1, care are acelasi comportament cu 
-- functia de mai sus.
ordonataNat1 :: [Int] -> Bool
ordonataNat1 [] = True
ordonataNat1 [x] = True
ordonataNat1 (x:xs) 
    | x > head xs   = False
    | otherwise     = ordonataNat1 xs 

-- 7. Scrieti o functie ordonata generică cu tipul
-- ordonata :: [a] -> (a -> a -> Bool) -> Bool
-- ordonata = undefined
-- care primeste ca argumente o listă de elemente si o relatie binară pe elementele respective. 
-- Functia întoarce True dacă oricare două elemente consecutive sunt în relatie.
-- a. Definiti functia ordonata prin orice metodă.
ordonata :: [a] -> (a -> a -> Bool) -> Bool
ordonata (x:xs) op = and [op a b| (a,b) <- zip (x:xs) xs]

-- b. Verificati definitia în interpretor pentru diferite valori:
-- • numere întregi cu relatia de ordine;
-- • numere întregi cu relatia de divizibilitate;
-- • liste (siruri de caractere) cu relatia de ordine lexicografică; observati că în Haskell este 
-- deja definită relatia de ordine lexicografică pe liste:
-- Prelude> [1,2] >= [1,3,4]
-- False
-- Prelude> "abcd"<"b"
-- True


-- 8. Definiti un operator *<*, asociativ la dreapta, cu precedenta 6, cu signatura
-- (*<*) :: (Integer, Integer) -> (Integer, Integer) -> Bool
-- care defineste o relatie pe perechi de numere întregi (alegeti voi relatia). Folosind functia
-- ordonata verificati dacă o listă de perechi este ordonată fată de relatia *<*.
(*<*) :: (Integer, Integer) -> (Integer, Integer) -> Bool
infixr 6 *<* 
(*<*) (a, b) (x, y) = ordonata [a,x] (<)  && ordonata [b, y] (<)


-- 9. Scrieti o functie compuneList de tip
-- compuneList :: (b -> c) -> [(a -> b)] -> [( a -> c)]
-- care primeste ca argumente o functie si o listă de functii si întoarce lista functiilor obtinute 
-- prin compunerea primului argument cu fiecare functie din al doilea argument.
-- *Main> :t compuneList (+1) [sqrt, (^2), (/2)]
-- Nu putem vizualiza direct rezultatul aplicării functiei compuneList. Atunci când
-- o functie întoarce functii (liste de functii, tupluri de functii, etc) ca valori, ele
-- nu pot fi vizualizate direct în interpretor. Pentru a verifica functionalitatea
-- trebuie să calculăm functiile în valori particulare.

compuneList :: (b -> c) -> [(a -> b)] -> [( a -> c)]
compuneList f = map (f .)
-- compuneList f lista = [ f.x | x <- lista]   --varianta fara map

--10
aplicaList :: a -> [(a -> b)] -> [b]
aplicaList a = map (\f -> f a)
-- aplicaList n lista = [fct n | fct <- lista]   --varianta fara map


-- aplicaList 9 [sqrt, (^2), (/2)]
-- aplicaList 9 (compuneList (+1) [sqrt, (^2), (/2)])
