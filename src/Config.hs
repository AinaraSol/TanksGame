module Config where

import Entities
import Control.Exception (try, IOException)
import Text.Read (readMaybe)
import System.Exit (die)

readConfig :: IO Config
readConfig = do
  result <- try (readFile "Configuration/config.txt") :: IO (Either IOException String)
  case result of
    Left _ -> die $ "Error fatal: No se pudo encontrar o leer el archivo de configuración"
    Right content -> case readMaybe content of
      Just config -> return config
      Nothing -> die $ "Error fatal: El archivo de configuración tiene un formato inválido."
