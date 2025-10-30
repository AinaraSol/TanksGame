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

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Juicy

main :: IO ()
main = do
    newState <- newGameState -- Con esto convertimos el IO GameState a GameState
    play
        (InWindow "Tanks Game" (round size*2,round size*2) (0,0))
        black
        60
        newState
        drawGame
        handleInput
        updateGame


updateGame :: Float -> GameState -> GameState
updateGame dt gameState = 
    case winner gameState of
      -- 1. Si ya hay un ganador, solo avanza el tiempo
      Just _ -> gameState { gameTime = gameTime gameState + dt }
      
      -- 2. Si no, ejecuta el juego
      Nothing -> 
        let
            -- Calculamos y aplicamos las acciones de cada tanque según su bot, 
            -- generando el tanque actualizado y los proyectiles disparados.
            actions = map (\t -> handleActions t (getBot gameState t)) (tanks gameState) 
            
            ws = worldSize gameState


            --Actualizamos las posiciones
            newTanks = [ reduceCooldown( updatePosition tank dt ws ) dt | (tank, _) <- actions, isRobotAlive tank ] --
            newProyectiles = concat [ projectile | (_, projectile) <- actions ]
            allProyectiles = proyectiles gameState ++ newProyectiles
            updatedProyectiles = updateProyectiles allProyectiles dt


            gameState' = gameState {proyectiles = updatedProyectiles}
            movedTanksGame = gameState' {tanks = newTanks}
            
            --Aplicamos las colisones
            gameAfterCollisions = checkCollisions movedTanksGame --

            updatedExplosions = [ explosion | explosion <- updateExplosions (explosions gameAfterCollisions) dt, explosionTime explosion <= 2] -- duracion de la explosion 1 segundo

            -- REVISAMOS SI HAY UN GANADOR
            livingTanks = filter isRobotAlive (tanks gameAfterCollisions) --
            
            -- Preparamos el tiempo final del frame
            finalTime = gameTime gameAfterCollisions + dt
            finalGame = gameAfterCollisions { gameTime = finalTime, explosions = updatedExplosions }

        in 
          case livingTanks of
             -- 3. Si se encuentra un ganador, graba SU ID y el TIEMPO ACTUAL
             [winTank] -> finalGame { winner = Just (idTank winTank, finalTime) }
             _         -> finalGame -- El juego sigue

handleInput :: Event -> GameState -> GameState
handleInput _ g = g

reduceCooldown :: Tank -> Float -> Tank
reduceCooldown t dt = t { shootCooldown = shootCooldown t - dt }

updateExplosions :: [Explosion] -> Float -> [Explosion]
updateExplosions explosions dt = map (updateExplosion dt) explosions

updateExplosion :: Float -> Explosion -> Explosion
updateExplosion dt explosion = explosion { explosionTime = explosionTime explosion + dt }
