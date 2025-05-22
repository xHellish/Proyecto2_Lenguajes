-- OcultarContraseña.hs
module OcultarContraseña (getPassword) where
import qualified System.Console.Haskeline as H
import Data.Maybe (fromMaybe)

-- | getPassword: muestra '*' por cada caracter ingresado
getPassword :: String -> IO String
getPassword prompt = H.runInputT H.defaultSettings $ do
    mInput <- H.getPassword (Just '*') prompt
    return $ fromMaybe "" mInput
