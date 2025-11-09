module Main where

import Bots 
import Constants
import Entities
import Geometry
import Tank
import Types
import Collisions
import Render
import NewGame
import Config
import Tournaments

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Color

import System.IO (writeFile)
import Control.Exception (try, SomeException)

main :: IO ()
main = do
    putStrLn "Inicializando archivo de estadísticas..."
    let statsHeader = "=== INICIO DE SESIÓN DE TORNEOS ===\n"
    -- writeFile para limpiar estadísticas
    result <- try (writeFile "estadisticas.txt" statsHeader) :: IO (Either SomeException ())
    case result of
        Left e  -> putStrLn $ "Advertencia: No se pudo limpiar estadisticas.txt: " ++ show e
        Right _ -> return () -- Todo bien

    newState <- newGameState 1
    playIO
        (InWindow "Tanks Game" (round getSizeX*2, round getSizeY*2) (0,0))
        black
        30
        newState
        (return . drawGame)
        handleInputIO 
        runTournament