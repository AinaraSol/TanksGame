module Config where

import Entities

readConfig :: FilePath -> IO Config
readConfig path = do
  contenido <- readFile path
  return (read contenido :: Config)
