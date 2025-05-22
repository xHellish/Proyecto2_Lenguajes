-- Registro y validacion de pin


module ModeloUsuarios (Usuario(..), crearUsuario, validarPin) where
import ManejoArchivos
import Cifrado
import System.IO


-- Definicion de un usuario
data Usuario = Usuario {
    nombreUsuario :: String,
    pinUsuario    :: String
} deriving (Show)

-- Crea un nuevo usuario con su nombre y pin
crearUsuario :: String -> String -> IO()
crearUsuario nombre pin = do


    -- primero se crea el objeto usuario 
    let usuario = Usuario { nombreUsuario = nombre, pinUsuario = pin }

    -- ahora se cifra para usarlo de nombre de archivo
    let nombreCifrado = cifrarConXor pin nombre

    -- ahora revisamos que el usuario no exista
    existe <- revisarNoExiste nombreCifrado "usuarios/"

    if not existe
        then putStrLn "Ya existe ese usuario"
        else do
            let nombreArchivo = "usuarios/" ++ nombreCifrado ++ ".txt"
            crearArchivo nombreArchivo
            putStrLn "Se creo su usario"


-- Revisar que el nombre de usuario no exista previamente en los usuarios con dicha contraseña
revisarNoExiste :: String -> FilePath  -> IO Bool
revisarNoExiste "" _ = error "El usuario no puede esta vacio"
revisarNoExiste _ "" = error "La clave no puede esta vacia"
revisarNoExiste nombre filepath = do

    -- primero listamos todos los txts 
    lista <- listarTxt "usuarios/"

    let archivo = nombre ++ ".txt"

    let coincidencias = filter (== archivo) lista

    --si no hubo coincidencias entonces no existe
    return (null coincidencias)


-- Valida si el pin ingresado es correcto para el usuario dado
validarPin :: String -> String -> IO Bool
validarPin nombre pin = do

    -- Funciona asi:
    -- internamente como tal los archivos no tienen el contenido de su nombre - pin
    -- la forma de saber si existe, es si existe un archivo con su nombre cifrado
    -- en caso de no encontrarlo hay dos posibles razones 
    --  -> el usuario con esa pin no ha sido registrado
    --  -> se equivoco digitando el pin

    let nombreCifrado = cifrarConXor pin nombre
    -- se usa la misma funcion que se aplicaba para revisar si ya existia 
    
    revisarNoExiste nombreCifrado "usuarios/"

