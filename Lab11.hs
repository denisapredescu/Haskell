import Data.List

type Name = String

-- Tipul Value reprezintă valorile asociate expresiilor. Valorile pot fi elemente Booleene, elemente 
-- întregi, functii sau erori.

data Value = VBool Bool
            |VInt Int
            |VFun (Value -> Value)
            |VError

data Hask = HTrue | HFalse
            |HIf Hask Hask Hask
            |HLit Int
            |Hask :==: Hask
            |Hask :+: Hask
            |HVar Name
            |HLam Name Hask
            |Hask :$: Hask
infix 4 :==:
infixl 6 :+:
infixl 9 :$:

-- Tipul HEnv reprezintă mediul de evaluare al expresiilor, asociind fiecărei variabile o valoare de tip Value.
type HEnv = [(Name, Value)]


-- 1. Afisarea valorilor expresiilor din Hask
-- Să se instantieze clasa Show pentru tipul Value, astfel încât să se afiseze valorile fără constructori. 
-- Functiile si erorile nu se pot afisa. Se va afisa un mesaj corespunzător.

instance Show Value where
    show (VBool a) = show a   
    show (VInt a)  = show a
    show (VFun _) = "functie"  --nu pot sa o verific !!!
    show (VError) = "eroare"

-- 2. Egalitate pentru valori
-- Să se instantieze clasa Eq pentru tipul Value, astfel încât să se verifice egalitatea între elemente de
-- tip Value. Functiile si erorile nu se pot compara. Putem întoarce o eroare folosind functia error.

instance Eq Value where
    VBool x == VBool y = x == y
    VInt x == VInt y  = x == y
    VFun _ == VFun _  = error "nu se pot compara 2 functii"  -- nu stiu cum sa le scriu nu doar eroare
    VError == VError = error "nu pot compara 2 erori"

-- 3. Evaluarea Expresiilor de tip Hask
-- Completati functia hEval astfel încât să evalueze fiecare expresie de tip Hask. Pentru a căuta o 
-- variabilă în mediul de evaluare, puteti folosi functia lookup din modulul Data.List sau să vă definiti
-- functia proprie.

hEval :: Hask -> HEnv -> Value
-- hEval = undefined
hEval HTrue r = VBool True
hEval HFalse r = VBool False
hEval (HIf c d e) r = hif (hEval c r) (hEval d r) (hEval e r)
                    where hif (VBool b) v w = if b then v else w
                          hif (VFun f) _ _ = VError
hEval (HLit a) r = VInt a
hEval (HVar nume) r = VError
hEval (a :==: b) r = heq (hEval a r) (hEval b r)
                    where heq (VInt a) (VInt b)  = VBool (a == b)
                          heq _ _ = VError

hEval (a :+: b) r = hadd (hEval a r) (hEval b r)
                    where hadd (VInt a) (VInt b) = VInt (a + b)
                          hadd _ _ = VError
hEval (a :$: b) r = hf (hEval a r) (hEval b r)
                    where hf (VFun f) x = f x
                          hf _ _ = VError
-- hEval (HLam nume a) r = 
--                         where hf (\x -> VFun nume) (a:r)
--                               hf _ _ = VError