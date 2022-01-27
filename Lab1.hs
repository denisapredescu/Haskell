-- 6. Sa se scrie urmatoarele funct, ii:
-- a) functie cu 2 parametri care calculeaza suma patratelor celor doua numere;
-- b) funct, ie cu un parametru ce întoarce mesajul “par” daca parametrul este 
-- par s, i “impar” altfel;
-- c) funct, ie care calculeaza factorialul unui numar;
-- d) funct, ie care verifica daca un primul parametru este mai mare decât dublul
-- celui de-al doilea parametru.

patrat :: Int -> Int -> Int
patrat x y = x * x + y* y 

paritate :: Int -> [Char]
paritate x = if rem x 2 == 0 then "par" else "impar"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

parametru :: Int -> Int -> Bool
parametru x y = if x > 2 * y then True else False

