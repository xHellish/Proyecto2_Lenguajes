-- Manejo de archivos

module ManejoArchivos (agregarAlFinal, leerLinea, abrirArchivo, cerrarArchivo, listarTxt, crearArchivo, eliminarLinea) where
import System.IO
import System.Directory (getDirectoryContents) --funcion para ver el contenido de un directorio
import Data.List (isSuffixOf) --Funcion para ver la terminacion de un string
import Control.DeepSeq (deepseq) --Se usa esta funcion para evitar el lazy loading de readFile

-- Esto escribe por la linea por la que va el archivo
agregarAlFinal :: FilePath -> String -> IO ()
agregarAlFinal archivo string = do
    handle <- openFile archivo AppendMode
    hPutStrLn handle string
    hClose handle


crearArchivo :: FilePath -> IO ()
crearArchivo rutaContenido = do
  handle <- openFile rutaContenido WriteMode
  hClose handle


abrirArchivo :: FilePath -> IO Handle
abrirArchivo ruta = openFile ruta ReadMode

cerrarArchivo :: Handle -> IO ()
cerrarArchivo handle = hClose handle

leerLinea :: Handle -> IO String
leerLinea handle = hGetLine handle


-- Esta funcion lista todos los txts de un directorio 
-- la usamos para ver todos los archivos de usuarios que hayan
listarTxt :: FilePath -> IO [FilePath]
listarTxt carpeta = do
  archivos <- getDirectoryContents carpeta
  let txtFiles = filter (".txt" `isSuffixOf`) archivos
  return txtFiles


-- fincion para eliminar una contraseña

eliminarLinea :: FilePath -> Int -> IO ()
eliminarLinea archivo n = do
    -- se saca el contenido del archivo
    contenido <- readFile archivo
    contenido `deepseq` return () -- Se usa esta funcion para evitar el lazy loading de readFile

    -- se pasan las lineas del archivo a una lista
    let lineas = lines contenido
    
    -- se elimina el indice asociado a la lista
    let nuevasLineas = eliminarEn n lineas

    -- se escribe el nuevo contenido al archivo
    writeFile archivo (unlines nuevasLineas)

-- Elimina el elemento en la posicion n de una lista
eliminarEn :: Int -> [String] -> [String]
eliminarEn _ [] = []
eliminarEn n (x:xs) 
    | n == 0 = xs 
    | otherwise = x : eliminarEn (n-1) xs