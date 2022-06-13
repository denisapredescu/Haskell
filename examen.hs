import Data.Char

-- Sa se scrie o functie care primeste ca parametru o lista de liste de numere intregi si doua numere intregi n si m, si verifica 
-- daca liniile care au toate elementele cuprinse intre n si m, au lungime para.
-- Pentru punctaj maxim scrieti si prototipul functiei cerute.

-- f [[1,2,3],[11,6,8,8],[2,3,4,5,6,7,8,9],[6,6,7,8,8,9]] 2 10 == True

f :: [[Int]] -> Int -> Int -> Bool
f [] n m = True    --daca ajunge la lista vida, inseamna ca nu a gasit nicio lista cu elem intre n si m de lungime impara
f (x : xs) n m 
    | filter (>n) x == x && filter (<m) x == x  = if mod (length x) 2 == 0 then f xs n m else False
    | otherwise   =  f xs n m 

-- ghci> f [[1,2,3],[11,6,8,8],[2,3,4,5,6,7,8,9],[6,6,7,8,8,9]] 2 10
-- True

--exemplu in care nu avem liste cu toate elementele cuprinse intre n si m
-- ghci> f [[1,2,3],[11,6,8,8],[2,3,4,5,6,7,8,9]] 2 10              
-- True

--exemplu in care lungimea nu este para
-- ghci> f [[1,2,3],[11,6,8,8],[2,3,4,5,6,7,8,9],[6,6,7,8,8]] 2 10  
-- False

--exemplu in care sunt mai multe liste care respecta toate cerintele
-- ghci> f [[1,2,3],[11,6,8,8],[3,3,4,5,6,7,8,9],[6,6,7,8,8,9]] 2 10
-- True

--exemplu in care sunt mai multe liste care au toate elementele cuprinse intre n si m, dar nu toate au lungime para
-- ghci> f [[1,2,3],[11,6,8,8],[3,4,5,6,7,8,9],[6,6,7,8,8,9]] 2 10  
-- False

--exemplu in care nu avem liste in lista
-- ghci> f [] 2 10                                                  
-- True

-- Se dau urmatoarele tipuri de date reprezentand dictionare. Un dictionar poate fi format dintr-o intrare (cu titlu si definitie) sau o lista de dictionare
-- (continand un titlu si lista de dictionare). 



type Name = String
type Def = String
data Dictionar = I Name Def
     | Ld Name [Dictionar]
   deriving Show

 

d1 = Ld "animal"[Ld "mamifer"[I "elefant" "acesta e un elefant", I "caine" "acesta este un caine", I "pisica" "aceasta este o pisica"], I "animale domestice" "definitie"]

d2 = Ld "Animal"[Ld "Mamifer"[I "Elefant" "acesta e un elefant",I "caIne" "acesta este un caine",I "piSIca" "aceasta este o pisica"],I "animale domestice" "definitie"]

d3 = Ld "animal"[Ld "mamifer"[I "elefant" "Acesta e un Elefant", I "caine" "acesta este un caine", I "pisica" "aceasta este o pisica"], I "animale domestice" "definitie"]

d4 = Ld "animal"[Ld "mamifer"[I "pisica" "aceasta este o pisica",I "elefant" "acesta e un elefant", I "caine" "acesta este un caine"], I "animale domestice" "definitie"]

 

-- a)     Sa se scrie o functie care primeste ca parametru un dictionar si intoarce numarul  intrarilor din acesta.

-- nrIntrari d1 = 4

nrIntrari :: Dictionar -> Int
nrIntrari (Ld n []) = 0
nrIntrari (I n d) = 1
nrIntrari (Ld n (x:xs)) = nrIntrari (x) + nrIntrari(Ld n xs)

-- ghci> nrIntrari d1
-- 4
-- ghci> nrIntrari (Ld "animal"[Ld "mamifer"[I "elefant" "Acesta e un Elefant", I "caine" "acesta este un caine", I "pisica" "aceasta este o pisica"], I "animale domestice" "definitie", I "animale domestice" "definitie"])
-- 5
-- ghci> nrIntrari (Ld "animal"[Ld "mamifer"[I "elefant" "Acesta e un Elefant", I "caine" "acesta este un caine", I "pisica" "aceasta este o pisica"]]) 
-- 3
-- ghci> nrIntrari (Ld "animal"[I "elefant" "Acesta e un Elefant"])                                                                                    
-- 1
-- ghci> nrIntrari (I "elefant" "Acesta e un Elefant")             
-- 1



-- b)    Sa se instantieze clasa Eq astfel incat sa se verifice egalitatea intre doua dictionare, comparand componentele lor in ordinea in care apar. Titlurile 
-- intrarilor sunt verificate fara a tine cont de litere mici sau mari.

--  I "caine" "animal" == I "CaiNe" "animal"   = True

-- I "Caine" "Animal" == I "Caine" "animal"  = False

-- d1== d2 = True

-- d1== d3 = False

-- d1== d4 = False

instance Eq Dictionar where
    I n1 def1 == I n2 def2 =  def1 == def2 && length n1 == length n2 && foldr (&&) True [toUpper x == toUpper y| (x,y) <- zip n1 n2 ]
    Ld n1 lista1 == Ld n2 lista2  =  lista1 == lista2


-- ghci> I "caine" "animal" == I "CaiNe" "animal"
-- True
-- ghci> I "Caine" "Animal" == I "Caine" "animal"  
-- False
-- ghci> I "caine" "animal" == I "CaiNe" "anim"   
-- False

-- daca nu as fi pus conditia ca lungimile titlurilor sa fie egale, ar fi returnat True din cauza zipului
-- ghci> I "caine" "animal" == I "CaiN" "animal" 
-- False

-- ghci> d1== d2 
-- True
-- ghci> d1== d3
-- False
-- ghci> d1== d4 
-- False





-- Să se scrie o funcție care transformă un text (șir de caractere) în varianta lui din limba păsărească.  Transformarea unui caracter se va face astfel:

--     - dacă e vocală, se adaugă un 'p' si vocala din nou, e.g., 'a' -> "apa"

--     - dacă nu e vocală, ramane nemodificat, e.g., 'p' -> "p"

-- Pentru punctaj maxim scrieti si prototipul functiei cerute.

-- Exemplu: "Mi-e foame" -> "Mipi-epe fopoapamepe"


pasareasca :: [Char] -> [Char]
pasareasca [] = []
pasareasca (x:xs)
    | elem x "aeiouAEIOU"  = x : 'p' : x : pasareasca xs
    | otherwise = x : pasareasca xs

-- ghci> pasareasca "Mi-e foame"
-- "Mipi-epe fopoapamepe"
-- ghci> pasareasca "LiTErE"    
-- "LipiTEpErEpE"
-- ghci> pasareasca ""    
-- ""

--varianta doar cu consoane
-- ghci> pasareasca "SqrT" 
-- "SqrT"
--varianta doar cu vocale
-- ghci> pasareasca "aeI"
-- "apaepeIpI"



data Optional a = Nada | Yep a
             deriving (Eq,Show)

instance Foldable Optional where
    foldr _ z Nada = z
    foldr f z (Yep x) = f x z

data Trivial = Trivial

-- instance Functor Trivial where
-- 	fmap f (Trivial a) = Trivial (f a)

-- instance Functor (Trivial) where
-- 	fmap _ Trivial = Trivial

data MyType a b =  MyType a b
                deriving (Eq, Show)

instance Functor (MyType a) where
    fmap f (MyType a b) = MyType a (f b)


-- daca ajunge la lista vida, inseamna ca nu a gasit nicio lista cu 
-- elem intre n si m de lungime impara
f' :: [[Int]] -> Int -> Int -> Bool
f' [] n m = True    --daca ajunge la lista vida, inseamna ca nu a gasit nicio lista cu elem intre n si m de lungime impara
f' (x : xs) n m
    | filter (>n) x == x && filter (<m) x == x  = if mod (length x) 2 == 0 then f' xs n m else False
    | otherwise   =  f' xs n m


data ConstAB a b = ConstAB b
                deriving (Eq,Show)


instance Foldable (ConstAB a) where
    foldMap f (ConstAB b) = f b
