import Data.Maybe
import Data.List

type Nume = String
data Prop  = Var Nume
            | F
            | T
            | Not Prop
            | Prop :|: Prop
            | Prop :&: Prop
            | Prop :->: Prop
            | Prop :<->: Prop
            deriving Eq
infixr 2 :|:
infixr 3 :&:
infixr 1 :->:  -- nu trebuie ca relatiile nou definite sa aiba prioritate in fata celorlalte
infixr 1 :<->:


-- Exercitiul 1

-- 1. (P ∨ Q) ∧ (P ∧ Q)
p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

-- 2. (P ∨ Q) ∧ (¬P ∧ ¬Q)
p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))

-- 3. (P ∧ (Q ∨ R)) ∧ ((¬P ∨ ¬Q) ∧ (¬P ∨ ¬R))
p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R")))

-- Exercitiul 2

-- Faceti tipul Prop instantă a clasei de tipuri Show, înlocuind conectivele Not, :|: si :&: cu ~, | si & si 
-- folosind direct numele variabilelor în loc de constructia Var nume.
instance Show Prop where
    show (Var p) = p
    show (F) = "False"
    show (T) = "True"
    show (Not p) = "(~" ++ show p ++ ")"
    show (p :|: q) = "(" ++ show p ++ "|" ++ show q ++ ")"
    show (p :&: q) = "(" ++ show p ++ "&" ++ show q ++ ")"
    show (p :->: q) = "(" ++ show p ++ "->" ++ show q ++ ")"
    show (p :<->: q) = "(" ++ show p ++ "<->" ++ show q ++ ")"

test_ShowProp :: Bool
test_ShowProp = show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"



-- Exercitiul 3
-- Definiti o functie eval care dat fiind o expresie logică si un mediu de evaluare, calculează valoarea de adevăr 
-- a expresiei.

-- lookup :: Eq a => a -> [(a,b)] -> Maybe b.
impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

test_eval = eval (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True
type Env = [(Nume, Bool)]

eval :: Prop -> Env -> Bool
eval (Var p) mediu= impureLookup p mediu 
eval F mediu = False
eval T mediu = True
eval (Not p) mediu = not (eval p mediu)
eval (p :|: q) mediu = eval p mediu || eval q mediu
eval ( p :&: q) mediu = eval p mediu && eval q mediu
eval ( p :->: q) mediu = eval p mediu --> eval q mediu
eval ( p :<->: q) mediu = eval p mediu <--> eval q mediu

-- Exercitiul 4
-- Definiti o functie variabile care colectează lista tuturor variabilelor dintr-o formulă.
-- Indicatie: folositi functia nub.

test_variabile = variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]

variabile :: Prop -> [Nume]

-- variabile p = nub (var p [])    --nu e ok pentru ca creez doua functii in loc de una
-- var (Var p) lista = p : lista 
-- var F lista = lista
-- var T lista = lista
-- var (Not p) lista =  var p lista
-- var (p :|: q) lista = var p lista ++ var q lista
-- var ( p :&: q) lista = var p lista ++ var q lista

variabile (Var p) = [p]
variabile F = []
variabile T = []
variabile (Not p) =  variabile p
variabile (p :|: q) = nub (variabile p ++ variabile q)
variabile ( p :&: q) = nub (variabile p ++ variabile q)
variabile ( p :->: q) = nub (variabile p ++ variabile q)
variabile ( p :<->: q) = nub (variabile p ++ variabile q)

-- Exercitiul 5
-- Dată fiind o listă de nume, definiti toate atribuirile de valori de adevăr posibile pentru ea.
envs :: [Nume] -> [Env]
envs =  sequence . map (\p -> [(p,False), (p, True)])

-- map (\p -> [(p,True), (p, False)]) xs  -- => determina liste pentru fiecare variabila cu toate posibilitatile (True / False)
-- sequence determina produsul cartezian

test_envs = envs ["P", "Q"] == [
                              [ ("P",False), ("Q",False)]
                            , [ ("P",False), ("Q",True)]
                            , [ ("P",True), ("Q",False)]
                            , [ ("P",True), ("Q",True)]
                               ]
-- Exercitiul 5
-- Definiti o functie satisfiabila care dată fiind o Propozitie verifică dacă aceasta este
-- satisfiabilă. Puteti folosi rezultatele de la exercitiile 4 si 5.

satisfiabila :: Prop -> Bool
satisfiabila p = foldr (||) False . map (eval p) $ mediu    --in map determin ce returneaza propozitia pentru fiecare mediu in parte
                where var = variabile p    -- determin variabile din prop -- ex 4
                      mediu = envs var     -- determin toate atribuirile de valori de adevar posibile -- ex 5 

test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False


-- Exercitiul 7
-- O propozitie este validă dacă se evaluează la True pentru orice interpretare a varibilelor.
-- O forumare echivalenta este aceea că o propozitie este validă dacă negatia ei este
-- nesatisfiabilă. Definiti o functie valida care verifică dacă o propozitie este validă.

valida :: Prop -> Bool
valida p = foldr (&&) True . map (eval p) $ mediu    --in map determin ce returneaza propozitia pentru fiecare mediu in parte
            where var = variabile p    -- determin variabile din prop -- ex 4
                  mediu = envs var     -- determin toate atribuirile de valori de adevar posibile -- ex 5 

test_valida1 = valida (Not (Var "P") :&: Var "Q") == False
test_valida2 = valida (Not (Var "P") :|: Var "P") == True

-- Exercitiul 9
-- Extindeti tipul de date Prop si functiile definite până acum pentru a include conectivele logice -> (implicatia)
-- si <-> (echivalenta), folosind constructorii :->: si :<->:.

-- trebuie sa creez functiile ce mimeaza implicatia si echivalenta
-- in loc de -> voi pune  -->
-- in loc de <-> voi pune <-->
    
(-->) :: Bool -> Bool -> Bool
True --> False = False
True --> True = True
_ --> _ = True 

(<-->) :: Bool -> Bool -> Bool
True <--> False = False
True <--> True = True
False <--> False = True
False <--> True = False

-- Exercitiul 10
-- Două propozitii sunt echivalente dacă au mereu aceeasi valoare de adevăr, indiferent de valorile variabilelor 
-- propozitionale. Scrieti o functie care verifică dacă două propozitii sunt echivalente.
echivalenta :: Prop -> Prop -> Bool
echivalenta p q = foldr (&&) True . map (eval (p :<->: q)) $ mediu
                  where var = variabile (p :&: q)           -- determin variabile din ambele prop -- ex 4
                        mediu = envs var                    -- determin toate atribuirile de valori de adevar posibile -- ex 5 


test_echivalenta1 = True == (Var "P" :&: Var "Q") `echivalenta` (Not (Not (Var "P") :|: Not (Var "Q")))
test_echivalenta2 = False == (Var "P") `echivalenta` (Var "Q")
test_echivalenta3 = True == (Var "R" :|: Not (Var "R")) `echivalenta` (Var "Q" :|: Not (Var "Q"))


-- varianta echivalenta cu cea de mai sus, dar nu am folosit :<->: si :&:
-- echivalenta p q = foldr (&&) True . map (\valori -> eval p valori == eval q valori) $ mediu
--                   where var = variabile p  ++ variabile q   -- determin variabile din ambele prop -- ex 4
--                         mediu = envs var                    -- determin toate atribuirile de valori de adevar posibile -- ex 5 


