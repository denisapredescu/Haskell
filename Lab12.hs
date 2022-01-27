{-
class Functor f where
fmap : : ( a -> b ) -> f a -> f b
-}

-- Scrieti instante ale clasei Functor pentru tipurile de date descrise mai jos.

newtype Identity a = Identity a   --pot dace instanta si pe newtype, nu doar pe data
instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a
instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

data Constant a b = Constant b
instance Functor (Constant a) where     -- pentru ca ii dau argument, tb sa pun paranteze
    fmap f (Constant a) = Constant (f a)

data Two a b = Two a b
instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)      -- aici pun constructorul / trec prin f doar variabila pe care nu o trec in instanta 

data Three a b c = Three a b c
instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b
instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

data Four'' a b = Four'' a a a b
instance Functor (Four'' a) where
    fmap f (Four'' a b c d) = Four'' a b c (f d)  -- nu trec prin f pe b si c pentru ca sunt de tip a => a -ul apare in constructor si deci nu se aplica functia pe el

data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where
    fmap f (Finance) = Finance
    fmap f (Desk a) = Desk a
    fmap f (Bloor a) = Bloor (f a) 

-- S-ar putea să fie nevoie să adăugati unele constrângeri la definirea instantelor
data LiftItOut f a = LiftItOut (f a)               -- f este o functie --=> fa rezultatul intors de f a
instance Functor f => Functor (LiftItOut f) where  -- f este functie, ca sa pot face instanta lui (LiftItOut f) tb. sa stiu ca f este de fapt si el un functor
    fmap h (LiftItOut fa) = LiftItOut (fmap h fa)

data Parappa f g a = DaWrappa (f a) (g a)  -- ambele functii se aplica pe a
instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap h (DaWrappa fa ga) = DaWrappa (fmap h fa) (fmap h ga) 

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
    fmap h (IgnoringSomething fa gb) = IgnoringSomething fa (fmap h gb) 

data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
    fmap h (Notorious go ga gt) = Notorious go ga (fmap h gt) 

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
    fmap h NoGoat = NoGoat
    fmap h (OneGoat a) = OneGoat (h a)
    fmap h (MoreGoats g1 g2 g3) = MoreGoats (fmap h g1) (fmap h g2) (fmap h g3)

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
    fmap h Halt = Halt
    fmap h (Print s a) = Print s (h a)
    fmap h (Read fa) = Read (fmap h fa)

-- ok: cand am produs pe o ramura sau functii ca parametru => apelez fmap in acea parte de cod
-- daca am doar variabile => apelez doar functia