-- La vaina de cifrar

module Cifrado (cifrarConXor) where

import Data.Bits (xor)
import Data.Char (ord, chr)


-- revisa que u char no sea ni retorno de carro ni salto de linea
esValido :: Int -> Bool
esValido c = c /= 10 && c /= 13

-- ajusta el valor de un char para que no sea ni retorno de carro ni salto de linea
ajustar :: Int -> Int
ajustar c
  | esValido c = c
  | otherwise  = ajustar ((c + 1) `mod` 256)  -- evitamos ciclo infinito

-- Cifra o descifra un texto usando una clave con XOR
cifrarConXor :: String -> String -> String
cifrarConXor "" _ = error "La clave no puede estar vacía"
cifrarConXor clave texto = zipWith (\c k -> chr (ajustar (ord c `xor` ord k))) texto (cycle clave)
-- esta funcion lo que hace es que con zipWith recibe dos strings y compara el ASCII de cada caracter con xor, luego lo pasa
-- nuevamente a char, y de ahí va formando un string que queda encriptado, y solo puede ser desencriptado si se aplica de nuevo
-- xor con la misma clave-