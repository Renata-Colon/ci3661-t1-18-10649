-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Renata Colon
--  Carnet: 1810649
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--  Para poder desarrollar la tarea use el manual de Haskell para entender el uso de
--  algunos keywords como "let-in" y "where", pero Gemini AI fue un guia para visualizar
--  los contextos de cuando aprovecharlos, corregir mis ideas y recibir orientacion de 
--  como solucionar algunos problemas que en este paradigma eran complicados de resolver 
--  (por lo menos para mi) como el segundo problema y el ultimo.
--
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1
esPalindromo :: String -> Bool
esPalindromo s = verificarIgualdadReversa(limpiarCadena s)

limpiarCadena :: String -> String
limpiarCadena [] = []
limpiarCadena (x:xs)
  | isAlphaNum x = toLower x : limpiarCadena xs
  | otherwise = limpiarCadena xs

verificarIgualdadReversa :: String -> Bool
verificarIgualdadReversa s = s == miReverse s

miReverse :: String -> String
miReverse xs = aux xs []
  where
    aux [] acc     = acc
    aux (y:ys) acc = aux ys (y : acc)

-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez: 
-- El algoritmo procesa la lista elemento por elemento de manera secuencial, avanzando a traves de la cola (ys) y en cada 
-- paso compara el numero actual con el max y min vigentes, como cada elemento se visita y evalua una unica vez antes de ser descartado
-- se completa el proceso en una sola pasada
-- 
mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor (x:xs) = Just (buscamaxmin (x, x) xs)

buscamaxmin :: Ord a => (a, a) -> [a] -> (a, a)
buscamaxmin (mx, mn) [] = (mx, mn)
buscamaxmin (mx, mn) (y:ys)
  | y > mx = buscamaxmin (y, mn) ys
  | y < mn = buscamaxmin (mx, y) ys
  | otherwise = buscamaxmin (mx, mn) ys

-- A3
leerHora :: String -> Either String (Int, Int)
leerHora [a, b, ':', c, d]
  | not (isDigit a && isDigit b && isDigit c && isDigit d) = Left "formato inválido"
  | horas < 0 || horas > 23 = Left "hora fuera de rango"
  | minutos < 0 || minutos > 59 = Left "minutos fuera de rango"
  | otherwise = Right (horas, minutos)
  where
    -- para usar digitToInt descartamos la opcion del keyword read
    horas = digitToInt a * 10 + digitToInt b
    minutos = digitToInt c * 10 + digitToInt d
leerHora _ = Left "formato inválido"

duracion :: String -> String -> Either String Int
duracion hora1 hora2 =
  case leerHora hora1 of
    Left err1 -> Left err1
    Right (h1, min1) ->
      case leerHora hora2 of
        Left err2 -> Left err2
        Right (h2, min2) -> 
          let inicio = h1 * 60 + min1
              fin = h2 * 60 + min2
              dif = fin - inicio 
          in if dif < 0 then Right(dif + 1440)
            else Right (dif)

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral umbral xs = 
  let (suma, cant) = foldl (\(s, c) x -> if x > umbral then (s + x, c + 1) else (s, c)) (0.0, 0 :: Int) xs 
  in if cant == 0
    then Nothing
    else Just (suma / fromIntegral cant)

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo (f, c) = 
  [(f + df, c + dc)
    | (df, dc) <- [(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)],
    f + df >= 1, f + df <= 8, c + dc >= 1, c + dc <= 8
  ]

enDosSaltos :: (Int, Int) -> Int
enDosSaltos pos = length (foldr insertarSinRepetir [] casillasTrasDosSaltos)
  where
    casillasTrasDosSaltos = [c2 | c1 <- movimientosCaballo pos, c2 <- movimientosCaballo c1]
    -- insertarSinRepetir es para añadir un elemento en acc solo si no existe en acc
    insertarSinRepetir x acc =
      if length (filter (== x) acc) == 0
        then x : acc
        else acc

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap f = foldr (\x acc -> f x : acc) [] 

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter p = foldr (\x acc -> if p x then x : acc else acc) []

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl f z xs = foldr (\x g acc -> g (f acc x)) id xs z
