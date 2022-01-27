import Data.Monoid
import Data.Foldable

-- 1. Implementati următoarele functii folosind foldMap si/sau foldr din clasa Foldable,
-- apoi testati-le cu mai multe tipuri care au instantă pentru Foldable


data BinaryTree a = Leaf a
                    | Node (BinaryTree a) (BinaryTree a)
                    deriving Show

instance Foldable BinaryTree where
    foldr = foldTree


-- foldMap :: Monoid m => (a -> m) -> t a -> m
-- fmap :: (a -> b) -> m a -> m b

--din curs---
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f i (Leaf x) = f x i
foldTree f i (Node l r) = foldTree f (foldTree f i r) l

treeI = Node(Node(Leaf 1)(Leaf 2))(Node(Leaf 3)(Leaf 4))
treeS = Node (Node(Leaf "1")(Leaf "2"))(Node(Leaf "3")(Leaf "4"))
--pana aici---

-- de la prof lab
my_elem :: (Foldable t, Eq a) => a -> t a -> Bool
my_elem e = getAny . (foldMap (Any . (== e)))     --merge daca ii dau doar un elem 

-- ghci> my_elem 4 (Just 3)
-- False

sasu_elem :: (Foldable t, Eq a) => a -> t a -> Bool
sasu_elem a = foldr (\x y -> (x == a) || y) False


my_null :: (Foldable t) => t a -> Bool
my_null = foldr (\_ _ -> False) True

---verifica daca am vreo cutie cu elemente  --- structura este goala
-- ghci> my_null (Just 5) 
-- False
-- ghci> my_null (Nothing)
-- True
-- ghci> my_null (Right 6)
-- False
-- ghci> my_null (Left "eroare")
-- True

--nu exista instanta de Eq, deci nu merge
-- my_null2 :: (Foldable t) => t a -> Bool
-- my_null2 x = if  (getSum $ foldMap Sum x) == 0 then True else False

my_null2 :: (Foldable t) => t a -> Bool     
my_null2 = getAll . foldMap (All . (const False))   ---constul facea ca sa intre valoarea in cutie


my_length :: (Foldable t) => t a -> Int
my_length = foldr (\x -> (+1)) 0 

my_length2 :: (Foldable t) => t a -> Int
my_length2 = getSum . foldMap (Sum . (const 1))

my_toList :: (Foldable t) => t a -> [a]
my_toList = foldr (\x y -> x : y) []


-- fold combină elementele unei structuri folosind structura de monoid a acestora.
my_fold :: (Foldable t, Monoid m) => t m -> m
my_fold = foldMap id      -- Hint: folositi foldMap

-- face operatia specifica Monoidului

-- ghci> my_fold $ Node(Node(Leaf (All True))(Leaf (All True)))(Node(Leaf (All False))(Leaf (All True)))
-- All {getAll = False}
-- ghci> my_fold $ Node(Node(Leaf (All True))(Leaf (All True)))(Node(Leaf (All True))(Leaf (All True))) 
-- All {getAll = True}


-- 2 .Scrieti instante ale lui Foldable pentru următoarele tipuri, implementand functia foldMap.

data Constant a b = Constant b
instance Foldable (Constant a) where
    foldMap f (Constant a) = f a

data Two a b = Two a b
instance Foldable (Two a) where
    foldMap f (Two a b) = f b

data Three a b c = Three a b c
instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

data Three' a b = Three' a b b
instance Foldable (Three' a) where
    foldMap f (Three' a b c) = (f b) <> (f c)

data Four' a b = Four' a b b b
instance Foldable (Four' a) where
    foldMap f (Four' a b c d) = (f b) <> (f c) <> (f d)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Foldable GoatLord where
    foldMap f NoGoat = mempty
    foldMap f (OneGoat a) = f a
    foldMap f (MoreGoats a b c) = (foldMap f a) <> (foldMap f b) <> (foldMap f c)

--functia f este de fapt un monoid
--relatia dintre monoizi este <>
--la final vom avea doar monoizi: deci nu pot intoarce aceleasi tipuri de date ca cele primite
--cand nu am pe ce sa aplic functia, pun mempty (elementul comun la monoizi)
--ATENTIE! foldMap intoarce un monoid
            -- functia ia ca mediu o cutie (aka data)


--exemplu Raluca:
exGoat = foldMap Sum (MoreGoats (OneGoat 4) NoGoat (MoreGoats (OneGoat 3) (OneGoat 7) NoGoat))