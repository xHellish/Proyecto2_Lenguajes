module GestionContras (menuGestionContraseñas) where
import ModeloUsuarios
import ManejoArchivos
import Cifrado
import System.IO
import Text.Read (readMaybe)
import System.Clipboard (setClipboardString)



-- Crud de contraseñas



menuGestionContraseñas :: Usuario -> IO ()
menuGestionContraseñas usuario = do

    putStrLn "\n===== MENU GESTIÓN DE CONTRASEÑAS ====="
    putStrLn "A Continuación se presentan sus contraseñas guardadas:\n"
    listarContraseñas usuario
    putStrLn "1. Guardar Contraseña"
    putStrLn "2. Modificar Valor"
    putStrLn "3. Eliminar Contraseña"
    putStrLn "4. Ver Contraseña"
    putStrLn "5. Cerrar Sesión"
    putStr "Seleccione una opcion: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> menuGuardarContraseña usuario >> menuGestionContraseñas usuario
        "2" -> menuModificarValor usuario >> menuGestionContraseñas usuario
        "3" -> menuEliminarContraseña usuario >> menuGestionContraseñas usuario
        "4" -> menuVerContraseña usuario >> menuGestionContraseñas usuario
        "5" -> putStrLn "Cerrando sesión. ¡Hasta luego!"
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


-- Opción 2: Modificar contraseña

menuModificarValor :: Usuario -> IO()
menuModificarValor user = do
    putStrLn "\n===== MODIFICAR VALOR ====="
    putStrLn "Que desea modificar?"
    putStrLn "1. Usuario"
    putStrLn "2. Contraseña"
    putStr "Seleccione una opcion: "
    hFlush stdout
    opcion <- getLine

    -- ahora se pide que servicio modificar
    putStrLn "\nA que servicio pertence?"
    listarContraseñas user
    putStr "Seleccione una opcion: "
    hFlush stdout
    servicio <- getLine


    -- finalmente se pide el nuevo valor
    putStr "Digite el nuevo valor: "
    hFlush stdout
    nuevoValor <- getLine
    
    let option = (read opcion :: Int) 
    let service = (read servicio :: Int) - 1 -- esto porque el usuario ve el servicio desde 1, pero en el archivo se guarda desde 0

    cambiarValor option service nuevoValor user

cambiarValor :: Int -> Int -> String -> Usuario -> IO()
cambiarValor opcion servicio nuevoValor usuario = do

    -- montamos la ruta del archivo
    let pin = pinUsuario usuario
    let nombre = nombreUsuario usuario
    let nombreArchivo = "usuarios/" ++ cifrarConXor pin nombre ++ ".txt"

    linea <- obtenerLinea servicio nombreArchivo
    let lista = words linea
    let listaDesencriptada = desencriptarLista lista pin

    -- se elimina informacion antigua
    eliminarLinea nombreArchivo servicio

    let nuevaLista = reemplazarEnIndice opcion nuevoValor listaDesencriptada 
    print listaDesencriptada 
    print nuevaLista
    -- una vez hecho esto se vuelve a guardar la nueva lista
    -- [servicio, user, pass]
    guardarContraseña (head nuevaLista) (nuevaLista !! 1) (nuevaLista !! 2) usuario


-- se usa para reemplezar con el nuevo valor
reemplazarEnIndice :: Int -> a -> [a] -> [a]
reemplazarEnIndice idx valor (x:xs) 
    | idx == 0 = valor : xs
    | otherwise = x : reemplazarEnIndice (idx - 1) valor xs 


-- Opción 3: Eliminar contraseña 

menuEliminarContraseña :: Usuario -> IO()
menuEliminarContraseña usuario = do
    putStrLn "\n===== ELIMINAR CONTRASEÑA ====="
    putStr "Digite el numero asociado a la contraseña: "
    hFlush stdout
    opcion <- getLine

    -- se revisa que si se haya metido un numero 
    case readMaybe opcion :: Maybe Int of
        Just numero -> do

            putStrLn $ "Numero recibido: " ++ show numero

            -- otra vez construimos el nombre del archivo
            let nombre = nombreUsuario usuario
            let pin = pinUsuario usuario

            -- ajustamos la ruta del archivo
            let nombreArchivo = "usuarios/" ++ cifrarConXor pin nombre ++ ".txt"

            eliminarLinea nombreArchivo (numero-1)

            return ()

        Nothing -> do

            putStrLn "Entrada invalida. Por favor, ingrese un numero."
            menuEliminarContraseña usuario  -- vuelve a pedir la entrada




-- Opción 4: Ver contraseña
menuVerContraseña :: Usuario -> IO()
menuVerContraseña usuario = do
    putStrLn "\n===== VER CONTRASEÑA ====="
    putStr "Digite el numero asociado a la contraseña: "
    hFlush stdout
    opcion <- getLine

    -- se revisa que si se haya metido un numero 
    case readMaybe opcion :: Maybe Int of
        Just numero -> do

            -- otra vez construimos el nombre del archivo
            let nombre = nombreUsuario usuario
            let pin = pinUsuario usuario

            -- ajustamos la ruta del archivo
            let nombreArchivo = "usuarios/" ++ cifrarConXor pin nombre ++ ".txt"

            lista <- verContraseña nombreArchivo (numero-1) pin

            putStrLn $ "Servicio: " ++ show (head lista) ++ "\nUsuario: " ++ lista !! 1 ++ "\nContraseña: " ++ lista !! 2

            putStrLn "Desea copiarla a portapapeles?"
            putStrLn "1. Si"
            putStrLn "2. No"
            putStr "Seleccione una opcion: "
            hFlush stdout
            opcion <- getLine

            case opcion of
                "1" -> do
                    setClipboardString (lista !! 2)  -- Copia la contraseña al portapapeles
                    putStrLn "Contraseña copiada al portapapeles."
                "2" -> putStrLn "No se copió la contraseña."
                _   -> putStrLn "Opción inválida."
            return ()

        Nothing -> do

            putStrLn "Entrada invalida. Por favor, ingrese un numero."
            menuVerContraseña usuario  -- vuelve a pedir la entrada

verContraseña :: FilePath -> Int -> String -> [String]
verContraseña nombreArchivo servicio pin = do

    linea <- obtenerLinea servicio nombreArchivo
    let lista = words linea
    let listaDesencriptada = desencriptarLista lista pin

    return listaDesencriptada

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