{- 
- PRACTICA 2
- Mora Espinosa Miroslava
- Estructuras Discretas 2025-2
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Distribution.Simple.Utils (xargs)
{-# HLINT ignore "Use max" #-}
{-# HLINT ignore "Use foldr" #-}

longitud :: [a] -> Int
longitud [] = 0  -- Caso base
longitud (_:xs) = 1 + longitud xs  -- Caso recursivo

sumaLista :: Num a => [a] -> a
sumaLista [] = 0 -- Caso base
sumaLista (x:xs) = x + sumaLista xs -- Caso recursivo

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento lista elemento True = elemento : lista  -- Agrega al principio de la lista
agregaElemento lista elemento False = lista ++ [elemento]  -- Agrega al final de la lista

maximoLista :: (Ord a, Num a) => [a] -> a
maximoLista [] = error "La lista está vacía"  -- Manejo del caso base
maximoLista [x] = x  -- Caso base
maximoLista (x:xs) = if x > maximoLista xs --si x es mayor, entonces x
                        then x
                        else maximoLista xs -- si no, sigue comparando

recuperarElemento :: [a] -> Int -> a
recuperarElemento [] _ = error "Índice no válido"  -- Si la lista está vacía, lanzamos un error
recuperarElemento (x:xs) 0 = x  
recuperarElemento (x:xs) n = if n < 0 || n >= length (x:xs)  -- En caso de que sea mayor o algo no valido.
                                then error "Índice no válido" 
                                else recuperarElemento xs (n - 1)  -- Caso recursivo

divisoresDeN :: Int -> [Int]
divisoresDeN = undefined

conjuntoLista :: Eq a => [a] -> [a]
conjuntoLista = undefined

soloPares :: [Int] -> [Int]
soloPares = undefined