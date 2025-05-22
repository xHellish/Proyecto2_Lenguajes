import ModeloUsuarios
import GestionContras
import OcultarContraseña (getPassword)
import Cifrado
import System.IO (hFlush, stdout)

main :: IO ()
main = menuPrincipal

-- Menú principal con repetición
menuPrincipal :: IO ()
menuPrincipal = do
    putStrLn "\n===== MENU PRINCIPAL ====="
    putStrLn "1. Crear usuario"
    putStrLn "2. Probar cifrado de un mensaje"
    putStrLn "3. Iniciar Sesion"
    putStrLn "4. Salir"
    putStr "Seleccione una opcion: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> opcionCrearUsuario >> menuPrincipal
        "2" -> opcionProbarCifrado >> menuPrincipal
        "3" -> iniciarSesion
        "4" -> putStrLn "Saliendo del programa. ¡Hasta luego!"
        _   -> putStrLn "Opcion invalida, intente de nuevo." >> menuPrincipal

-- Opción 1: Crear usuario
opcionCrearUsuario :: IO ()
opcionCrearUsuario = do
    putStrLn "\n=== CREAR USUARIO ==="
    putStr "Ingrese el nombre del usuario: "
    hFlush stdout
    nombre <- getLine
    pinCreado <- getPassword "Ingrese el PIN del usuario: "

    crearUsuario nombre pinCreado

-- Opción 1: Iniciar Sesion 

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

    

-- Opción 2: Probar el cifrado simple
opcionProbarCifrado :: IO ()
opcionProbarCifrado = do
    putStrLn "\n=== PRUEBA DE CIFRADO ==="
    putStr "Ingrese la clave (PIN): "
    hFlush stdout
    clave <- getLine
    putStr "Ingrese el mensaje a cifrar: "
    hFlush stdout
    mensaje <- getLine

    let cifrado = cifrarConXor clave mensaje
    let descifrado = cifrarConXor clave cifrado

    putStrLn ("Mensaje cifrado: " ++ cifrado)
    putStrLn ("Mensaje descifrado: " ++ descifrado)
