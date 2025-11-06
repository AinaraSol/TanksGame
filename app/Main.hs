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
        (InWindow "Tanks Game" (round sizeX*2,round sizeY*2) (0,0))
        black
        30
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
            updatedObstacles = updateObstacles (obstacles gameAfterCollisions) dt

            obstaclesExplosions = [ Explosion (obstaclePosition obs) 0 | obs <- updatedObstacles, obstacleTime obs < Just 0 ]

            -- Aplicamos el daño de las minas a los tanques que estan en el radio de alcance de las minas
            tanksAfterExplosions = applyMineDamage (tanks gameAfterCollisions) updatedObstacles

            gameAfterExplosions = gameAfterCollisions { tanks = tanksAfterExplosions }

            newObstacles = [obs | obs <- updatedObstacles, obstacleTime obs >= Just 0]

            newUpdatedExplosions = updatedExplosions ++ obstaclesExplosions

            -- REVISAMOS SI HAY UN GANADOR
            livingTanks = filter isRobotAlive (tanks gameAfterExplosions) --
            
            -- Preparamos el tiempo final del frame
            finalTime = gameTime gameAfterExplosions + dt
            finalGame = gameAfterExplosions { gameTime = finalTime, explosions = newUpdatedExplosions, obstacles = newObstacles }

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

updateObstacles :: [Obstacle] -> Float -> [Obstacle]
updateObstacles obstacles dt = map (updateObstacle dt) obstacles

updateObstacle :: Float -> Obstacle -> Obstacle
updateObstacle dt obstacle = obstacle { obstacleTime = newTime (obstacleTime obstacle) }
  where 
    newTime Nothing = Nothing
    newTime (Just t)
      | obstacleClass obstacle == 3 && obstacleTrigger obstacle == False = Just t
      | obstacleTrigger obstacle == True = Just (t - dt)
      | otherwise = Just (t + dt)

