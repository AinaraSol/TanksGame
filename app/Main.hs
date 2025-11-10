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
import Torneos
import Statistics

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Color

main :: IO ()
main = do
    clearStatisticsFile
    newState <- newGameState 1 [] -- Inicializar el estado del juego con el primer torneo y estadísticas vacías
    playIO
        (InWindow "Tanks Game" (round getSizeX*2, round getSizeY*2) (0,0))
        black
        30
        newState
        (return . drawGame)
        handleInputIO 
        runTournament