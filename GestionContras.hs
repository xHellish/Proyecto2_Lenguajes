module GestionContras (menuGestionContraseñas) where
import ModeloUsuarios
import ManejoArchivos
import Cifrado
import System.IO
import Text.Read (readMaybe)


-- Crud de contraseñas



menuGestionContraseñas :: Usuario -> IO ()
menuGestionContraseñas usuario = do

    putStrLn "\n===== MENU GESTIÓN DE CONTRASEÑAS ====="
    putStrLn "A Continuación se presentan sus contraseñas guardadas:\n"
    listarContraseñas usuario
    putStrLn "1. Guardar Contraseña"
    putStrLn "2. Modificar Contraseña"
    putStrLn "3. Eliminar Contraseña"
    putStrLn "4. Cerrar Sesión"
    putStr "Seleccione una opcion: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> menuGuardarContraseña usuario >> menuGestionContraseñas usuario
        "2" -> menuEliminarContraseña usuario
        "3" -> menuEliminarContraseña usuario >> menuGestionContraseñas usuario
        "4" -> putStrLn "Cerrando sesión. ¡Hasta luego!"
        _   -> putStrLn "Opcion invalida, intente de nuevo." >> menuGestionContraseñas usuario



-- Opción 1: guardar contraseña nueva

menuGuardarContraseña :: Usuario -> IO()
menuGuardarContraseña usuario = do
    putStrLn "\n===== GUARDAR CONTRASEÑA ====="
    putStr "Digite el servicio: "
    hFlush stdout
    servicio <- getLine

    putStr "Digite el usuario: "
    hFlush stdout
    user <- getLine

    putStr "Digite la contraseña: "
    hFlush stdout
    pass <- getLine

    guardarContraseña servicio user pass usuario
    putStrLn "Se guardó la contraseña exitosamente"


guardarContraseña :: String -> String -> String -> Usuario -> IO()
guardarContraseña servicio user pass usuario = do

    -- montamos la ruta del archivo
    let pin = pinUsuario usuario
    let nombre = nombreUsuario usuario
    let nombreArchivo = "usuarios/" ++ cifrarConXor pin nombre ++ ".txt"

    --Ciframos toda la informacion
    let servicioCifrado = cifrarConXor pin servicio
    let userCifrado = cifrarConXor pin user
    let passCifrada = cifrarConXor pin pass

    --preparamos la linea
    let linea = servicioCifrado ++ " " ++ userCifrado ++ " " ++ passCifrada

    --se escribe la linea en el archivo 
    agregarAlFinal nombreArchivo linea


-- Opción 2: Eliminar contraseña 
menuEliminarContraseña :: Usuario -> IO()
menuEliminarContraseña usuario = do
    putStrLn "\n===== ELIMINAR CONTRASEÑA ====="
    putStr "Digite el numero asociado a la contraseña: "
    hFlush stdout
    opcion <- getLine

    -- se revisa que si se haya metido un numero 
    case readMaybe opcion :: Maybe Int of
        Just numero -> do

            putStrLn $ "Número recibido: " ++ show numero

            -- otra vez construimos el nombre del archivo
            let nombre = nombreUsuario usuario
            let pin = pinUsuario usuario

            -- ajustamos la ruta del archivo
            let nombreArchivo = "usuarios/" ++ cifrarConXor pin nombre ++ ".txt"

            eliminarLinea nombreArchivo (numero-1)

            return ()

        Nothing -> do

            putStrLn "Entrada inválida. Por favor, ingrese un número."
            menuEliminarContraseña usuario  -- vuelve a pedir la entrada




--- Funciones para listar las contraseñas
listarContraseñas :: Usuario -> IO ()
listarContraseñas usuario = do

    -- extraemos los datos del usuario 
    let nombre = nombreUsuario usuario
    let pin = pinUsuario usuario

    -- ajustamos la ruta del archivo
    let nombreArchivo = "usuarios/" ++ cifrarConXor pin nombre ++ ".txt"

    -- lo abrimos
    handle <- abrirArchivo nombreArchivo

    -- ahora se va a mandar el handle a una funcion auxiliar que imprime a todos 
    let index = 1
    putStrLn "========================="
    listarContraseñasAux handle pin index
    putStrLn "=========================\n"
    -- Siempre recordar cerrarlo
    cerrarArchivo handle

listarContraseñasAux :: Handle -> String -> Int -> IO ()
listarContraseñasAux handle pin index = do

    fin <- hIsEOF handle
    if fin
        then return ()
        else do
            linea <- leerLinea handle

            let lista = words linea

            let listaDesencriptada = desencriptarLista lista pin

            -- no se muestran las letras del user luego de la cuarta letra
            let user = encryptUser (listaDesencriptada !! 1) 
            -- se imprime la contraseña con *
            print ( listaDesencriptada !! 2)
            let pass = replicate (length (listaDesencriptada !! 2)) '*'

            putStrLn (show index ++". " ++"Servicio: "++ show (head listaDesencriptada) ++ " Usuario: " ++ user ++ " Contraseña: " ++ pass)


            listarContraseñasAux handle pin (index+1)

encryptUser :: String -> String
encryptUser user
    | length user <= 4 = user
    | otherwise = take 4 user ++ replicate (length user - 4) '*'
  
desencriptarLista :: [String] -> String -> [String]
desencriptarLista [] _  = []
desencriptarLista (x:xs) pin = cifrarConXor pin x : desencriptarLista xs pin