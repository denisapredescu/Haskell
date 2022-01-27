import Data.Char

-- 1. Putem roti o lista luând o parte de la început si adăugând-o la final:
-- Main> rotate 3 "ABCDEFGHIJKLMNOPQRSTUVWXYZ" == "DEFGHIJKLMNOPQRSTUVWXYZABC"
-- Deschideti fisierul lab6.hs si completati functia rotate :: Int -> [Char] -> [Char].
-- Pentru un număr n, n > 0 si n<lungimea listei, functia va roti lista cu n elemente. Functia trebuie să 
-- arunce o eroare daca numărul n este negativ sau prea mare (hint: folositi functia error).

rotate :: Int -> [Char] -> [Char]
rotate n xs
    | n < 0   = error "n este negativ"
    | n > length xs = error "n este prea mare"
    | otherwise  = drop n xs ++ take n xs

-- 2. Observati functia prop_rotate. Ce testează? Cum evită această functie aruncarea erorii?

prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                    where l = length str
                          m = if l == 0 then 0 else k `mod` l

-- Erorile sunt evitate prin valoarea data variabilei m astfel incat diferenta (l - m) returneaza intotdeauna
-- o valoare pozitiva (>=0) si mai mica decat lungimea stringului. Se mentioneaza ca l reprezinta lungimea stringului,
-- iar m va lua o valoare redusa a lui k astfel incat sa nu depaseasca l.
-- Nu conteaza exact modul in care este redus k intrucat functia returneaza mereu True pentru ca 
-- in fapt rotirea se face cu l (= lungimea stringului) caractere

-- 3. Folosind functia rotate, scrieti o functie makeKey :: Int -> [(Char, Char)] care intoarce cheia de 
-- criptare cu o anumită deplasare pentru lista de litere mari ale alfabetului englez.
-- Main> makeKey 5
-- [('A','F'),('B','G'),('C','H'),('D','I'),('E','J'),('F','K'),
-- ('G','L'),('H','M'),('I','N'),('J','O'),('K','P'),('L','Q'),
-- ('M','R'),('N','S'),('O','T'),('P','U'),('Q','V'),('R','W'),
-- ('S','X'),('T','Y'),('U','Z'),('V','A'),('W','B'),('X','C'),
-- ('Y','D'),('Z','E')]

makeKey :: Int -> [(Char, Char)]
makeKey n = [(x, y) | (x,y) <- zip ['A'..'Z'] (rotate n ['A'..'Z'])]

-- 4. Scrieti o functie lookUp :: Char -> [(Char, Char)] -> Char care caută o pereche după prima componentă
-- si întoarce a doua componentă a acesteia. Dacă nu există o pereche cu caracterul căutat pe prima pozitie,
-- functia întoarce caracterul dat ca parametru.
-- lookUp 'B' [('A', 'F'), ('B', 'G'), ('C', 'H')] == 'G'
-- lookUp '9' [('A', 'X'), ('B', 'Y'), ('C', 'Z')] == '9'

lookUp :: Char -> [(Char, Char)] -> Char
lookUp c [] = c
lookUp c (x:xs) 
    | c == fst x  = snd x
    | otherwise   = lookUp c xs

-- 5. Scrieti o functie encipher :: Int -> Char -> Char care criptează un caracter folosind cheia dată de 
-- o deplasare dată ca parametru.
-- encipher 5 'C' == 'H'
-- encipher 7 'Q' == 'X'

encipher :: Int -> Char -> Char
encipher n c =  lookUp c (makeKey n)

-- 6. Pentru a fi criptat, textul trebuie să nu contină semne de punctuatie si să fie scris cu litere mari.
-- Scrieti o functie normalize :: String -> String care normalizează un sir, transformând literele mici în 
-- litere mari si eliminând toate caracterele care nu sunt litere sau cifre.
-- normalize "July 4th!" == "JULY4TH"
-- isDigit -- testeaza daca argumentul e o cifra
-- isAlpha -- testeaza daca argumentul este din alfabet

normalize :: String -> String
normalize str =  [toUpper x | x <- str, isDigit x || isAlpha x]

-- 7. Scrieti o functie encipherStr :: Int -> String -> String care normalizează un sir si îl criptează 
-- folosind functiile definite anterior.
-- encipherStr 5 "July 4th!" == "OZQD4YM"

encipherStr :: Int -> String -> String 
encipherStr n str = map (encipher n) norm
                    where norm = normalize str

-- 8. Scrieti o functie reverseKey :: [(Char, Char)] -> [(Char, Char)] pentru a inversa cheia de criptare, 
-- schimbând componentele din fiecare pereche între ele.
-- reverseKey [('A', 'G'), ('B', 'H') , ('C', 'I')] == [('G', 'A'), ('H', 'B') , ('I', 'C')]

reverseKey :: [(Char, Char)] -> [(Char, Char)] 
reverseKey = map (\(x, y) -> (y, x)) 

-- 9. Scrieti functiile
-- decipher :: Int -> Char -> Char
-- decipherStr :: Int -> String -> String
-- pentru a decripta un caracter si un string folosind cheia generată de o deplasare dată. Functia va lăsa 
-- nemodificate cifrele si spatiile, dar va sterge literele mici sau alte caractere.
-- decipherStr 5 "OZQD4YM" == "JULY4TH"

decipher :: Int -> Char -> Char
decipher n c = lookUp c (reverseKey (makeKey n))

decipherStr :: Int -> String -> String
decipherStr n = map (decipher n)