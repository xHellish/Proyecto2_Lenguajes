-- Main.hs
module Main where

import OcultarContraseña (getPassword)
import System.IO (hFlush, stdout)

-- ========================================
-- Constantes 
-- ---------------------------------------
usuarioCorrectoSistema :: String
usuarioCorrectoSistema = "test_user"  -- test para el usuario

pinCorrectoSistema :: String
pinCorrectoSistema = "1111"  -- test para el pin

-- ========================================
-- Funciones 
-- ---------------------------------------

-- ========================================
-- Bucle menú principal
bucle_menu_principal :: IO ()
bucle_menu_principal = do
    putStrLn "----------=========================----------"
    putStrLn "                MENÚ PRINCIPAL      "
    putStrLn "----------=========================----------"
    putStrLn "1. Registrarse"
    putStrLn "2. Iniciar sesión"
    putStrLn "3. Salir"
    putStr   "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine
    manejarOpcion opcion

-- ========================================
-- Manejar elección del menú
manejarOpcion :: String -> IO ()
manejarOpcion "1" = registrarse
manejarOpcion "2" = iniciar_sesion
manejarOpcion "3" = putStrLn "\nSaliendo del programa. Hasta luego."
manejarOpcion _   = do
    putStrLn "\nOpción no válida. Intente nuevamente.\n"
    bucle_menu_principal

-- ========================================
-- Registrarse
registrarse :: IO ()
registrarse = do
    putStr "Ingrese su nombre: "
    hFlush stdout
    nuevo_user <- getLine

    -- Usar getPassword para ocultar la contraseña
    nueva_pass <- getPassword "Cree su PIN: "
    putStrLn ("\nHola, " ++ nuevo_user ++ "! Se ha creado su usuario con la contraseña " ++ replicate (length nueva_pass) '*')

-- ========================================
-- Validar usuario y PIN
validar_usuario :: String -> String -> Bool
validar_usuario user pin = user == usuarioCorrectoSistema && pin == pinCorrectoSistema

-- ========================================
-- Acceso exitoso
ingresar_en_sistema :: IO ()
ingresar_en_sistema = putStrLn "\nBienvenido al sistema." -- -> TO DO

-- ========================================
-- Acceso denegado
acceso_denegado :: IO ()
acceso_denegado = putStrLn "\nAcceso denegado. Usuario o PIN incorrecto." -- -> TO DO


-- ========================================
-- Iniciar sesion
iniciar_sesion :: IO ()
iniciar_sesion = do
    putStr "Nombre de usuario: "
    hFlush stdout
    usuario <- getLine

    -- Contraseña con el módulo para ocultarla
    pass <- getPassword "Ingrese su PIN: "
    putStrLn ("Iniciando sesión...")

    if validar_usuario usuario pass
        then ingresar_en_sistema
        else acceso_denegado

-- ========================================
-- MAIN
main :: IO ()
main = do
    -- Usar getPassword para ocultar el PIN

    bucle_menu_principal
    
