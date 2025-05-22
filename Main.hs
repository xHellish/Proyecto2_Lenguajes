import ModeloUsuarios
import GestionContras
import OcultarContraseña (getPassword)
import Cifrado
import System.IO (hFlush, stdout)

main :: IO ()
main = menuPrincipal

-- Menu principal con repeticion
menuPrincipal :: IO ()
menuPrincipal = do
    putStrLn "\n===== MENU PRINCIPAL ====="
    putStrLn "1. Crear usuario"
    putStrLn "2. Iniciar Sesion"
    putStrLn "3. Salir"
    putStr "Seleccione una opcion: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> opcionCrearUsuario >> menuPrincipal
        "2" -> iniciarSesion
        "3" -> putStrLn "Saliendo del programa. ¡Hasta luego!"
        _   -> putStrLn "Opcion invalida, intente de nuevo." >> menuPrincipal

-- Opcion 1: Crear usuario
opcionCrearUsuario :: IO ()
opcionCrearUsuario = do
    putStrLn "\n=== CREAR USUARIO ==="
    putStr "Ingrese el nombre del usuario: "
    hFlush stdout
    nombre <- getLine
    pinCreado <- getPassword "Ingrese el PIN del usuario: "

    crearUsuario nombre pinCreado

-- Opcion 2: Iniciar Sesion 

iniciarSesion :: IO()
iniciarSesion = do 

    putStrLn "\n=== INICIAR SESION ==="
    putStr "Ingrese el nombre del usuario: "
    hFlush stdout
    nombre <- getLine
    pin <- getPassword "Ingrese el PIN del usuario: "

    pinCorrecto <- validarPin nombre pin 
    if not pinCorrecto
        then do
            let usuario = Usuario { nombreUsuario = nombre, pinUsuario = pin }
            menuGestionContraseñas usuario
        else 
            putStrLn "Credenciales no dan resultados"

    menuPrincipal

    
