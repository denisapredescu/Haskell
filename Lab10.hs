-- 1. Expresii si Arbori

-- Se dau următoarele tipuri de date reprezentând expresii si arbori de expresii:
data Expr = Const Int -- integer constant
            | Expr :+: Expr -- addition
            | Expr :*: Expr -- multiplication
            deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
            | Node Operation Tree Tree -- branch
            deriving (Eq, Show)

-- 1.1. Să se instantieze clasa Show pentru tipul de date Expr, astfel încât să se afiseze mai simplu expresiile.

instance Show Expr where
    show (Const a) = show a  --trebuie sa il afisez pe a (adica show a /= a)
    show (a :+: b) = "(" ++ show a ++ "+" ++ show b ++")"
    show (a :*: b) = "(" ++ show a ++ "*" ++ show b ++")"

-- 1.2. Să se scrie o functie evalExp :: Expr -> Int care evaluează o expresie determinând valoarea acesteia.
evalExp :: Expr -> Int
evalExp (Const a) = a
evalExp (a :+: b) = evalExp a + evalExp b
evalExp (a :*: b) = evalExp a * evalExp b 

-- Exemplu:
exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)
test11 = evalExp exp1 == 6
test12 = evalExp exp2 == 14
test13 = evalExp exp3 == 13
test14 = evalExp exp4 == 16

-- 1.3. Să se scrie o functie evalArb :: Tree -> Int care evaluează o expresie determinând valoarea acesteia.
evalArb :: Tree -> Int
evalArb (Lf l) = l 
evalArb (Node Add t1 t2) = (evalArb t1) + (evalArb t2)
evalArb (Node Mult t1 t2) = (evalArb t1) * (evalArb t2)

-- Exemplu
arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)
test21 = evalArb arb1 == 6
test22 = evalArb arb2 == 14
test23 = evalArb arb3 == 13
test24 = evalArb arb4 == 16

-- 1.4. Să se scrie o functie expToArb :: Expr -> Tree care transformă o expresie în arborele corespunzător.
expToArb :: Expr -> Tree
expToArb (Const a) = Lf a
expToArb (a :+: b) = Node Add (expToArb a) (expToArb b)
expToArb (a :*: b) = Node Mult (expToArb a) (expToArb b)


-- 2. Clasa Collection
-- In acest exercitiu vom exersa manipularea listelor si tipurilor de date prin implementarea catorva colectii de 
-- tip tabela asociativa cheie-valoare.
-- Aceste colectii vor trebui sa aiba urmatoarele facilitati
-- • crearea unei colectii vide
-- • crearea unei colectii cu un element
-- • adaugarea/actualizarea unui element intr-o colectie
-- • cautarea unui element intr-o colectie
-- • stergerea (marcarea ca sters a) unui element dintr-o colectie
-- • obtinerea listei cheilor
-- • obtinerea listei valorilor
-- • obtinerea listei elementelor
class Collection c where
    empty :: c key value
    singleton :: key -> value -> c key value
    insert :: Ord key => key -> value -> c key value -> c key value
    clookup :: Ord key => key -> c key value -> Maybe value    --exista o functie predefinita lookup => nu o pot instantia
    delete :: Ord key => key -> c key value -> c key value
    keys :: c key value -> [key]
    values :: c key value -> [value]
    toList :: c key value -> [(key, value)]
    fromList :: Ord key => [(key,value)] -> c key value

    keys c = [k | (k,v) <- toList c]
    values c = [v | (k,v) <- toList c]

    fromList [] = empty
    fromList ((k,v):lista) = insert k v (fromList lista)

-- 2.1. Adaugati definitii implicite (in functie de functiile celelalte) pentru
-- a. keys
-- b. values
-- c. fromList

-- 2.2. Fie tipul listelor de perechi de forma cheie-valoare:
-- newtype PairList k v = PairList { getPairList :: [(k, v)] }
-- Faceti PairList instanta a clasei Collection

newtype PairList k v = PairList { getPairList :: [(k, v)] }

instance Collection PairList where
    empty = PairList []
    singleton k v = PairList [(k, v)]

    --este vorba de tupluri cheie-valoare => nu pot avea de doua ori aceeasi cheie
    insert k v (PairList p) = PairList ((k,v) : [(a,b) | (a,b) <- p, a /= k])
    
    clookup k (PairList p) = lookup k p
    delete k p = PairList [(key, value) | (key, value) <- toList p, key /= k]
    toList (PairList p) = p

    --cele care vor urma nu ar mai fi trebuit fi instantiate pentru ca PairList este instanta a clasei Collection,
    --iar functiile keys, values si fromList au fost deja definite in clasa
    keys p = [fst t | t <- toList p]
    values p = [snd t | t <- toList p]

    fromList [] = empty
    fromList ((k,v):p) = insert k v (fromList p)


-- 2.3. Fie tipul arborilor binari de cautare (ne-echilibrati):

data SearchTree key value = Empty
                            | BNode (SearchTree key value) -- elemente cu cheia mai mica
                                    key -- cheia elementului
                                    (Maybe value) -- valoarea elementului
                                    (SearchTree key value) -- elemente cu cheia mai mare

-- Observati ca tipul valorilor este Maybe value. Acest lucru se face pentru a reduce timpul operatiei de stergere 
-- prin simpla marcare a unui nod ca fiind sters. Un nod sters va avea valoarea Nothing.

-- Faceti SearchTree instanta a clasei Collection.

instance Collection SearchTree where
    empty = Empty
    singleton k v = BNode Empty k (Just v) Empty

    --inserez un nod nou doar daca nu gasesc cheia => daca o gasesc, ii schimb doar valoarea
    insert k v Empty = singleton k v
    insert k v (BNode left key value right)
        | k == key  = BNode left key (Just v) right
        | k < key   = BNode (insert k v left) key value right
        | otherwise = BNode left key value (insert k v right)

    clookup k Empty = Nothing
    clookup k (BNode left key value right)
        | k == key  = value
        | k < key   = clookup k left    -- nu imi mai pasa ce se intampla pe cealalta ramura => eu parcurg in adancime pana dau de cheia dorita
        | otherwise = clookup k right  -- => daca nu dau, intorc Nothing, altfel valoarea care se gasea la acea cheie


    delete k Empty = Empty
    delete k (BNode left key value right)
        | k == key  = BNode left key Nothing right
        | k < key   = BNode (delete k left) key value right 
        | otherwise = BNode left key value (delete k right)

    toList Empty = []
    toList (BNode left key Nothing right) = toList left ++ toList right    -- Nothing inseamna ca nodul respectiv este de fapt sters => nu trebuie pus in lista (nu exista acel nod)
    toList (BNode left key (Just value) right) = toList left ++ [(key, value)] ++ toList right

    --la fel ca la PairList, functiile keys, values si fromList nu este necesar sa fie definite din nou
    keys Empty = []
    keys (BNode left key value right) = [key] ++ keys left ++ keys right

    values tree = [(v) | (k,v) <- toList tree]   -- puteam sa fac la fel si pt keys
    
    fromList [] = Empty
    fromList ((k,v):list) = insert k v (fromList list)


