-- La vaina de cifrar

module Cifrado (cifrarConXor) where

import Data.Bits (xor)
import Data.Char (ord, chr)

-- Cifra o descifra un texto usando una clave con XOR
cifrarConXor :: String -> String -> String
cifrarConXor "" _ = error "La clave no puede estar vacia"
cifrarConXor clave texto = zipWith (\c k -> chr (ord c `xor` ord k)) texto (cycle clave)
-- esta funcion lo que hace es que con zipWith recibe dos strings y compara el ASCII de cada caracter con xor, luego lo pasa
-- nuevamente a char, y de ahi va formando un string que queda encriptado, y solo puede ser desencriptado si se aplica de nuevo
-- xor con la misma clave-