module Tournaments where

import Entities
import NewGame
import Constants
import Bots
import Tank
import Collisions

import Graphics.Gloss.Interface.IO.Game

runTournament :: Float -> GameState -> IO GameState
runTournament dt gameState = 
    case winner gameState of
      -- Si ya hay un ganador
      Just (winId, winTime) -> 
        let elapsed = gameTime gameState - winTime
            maxTournaments = getNumTournaments
            currentTournament = tournament gameState
        in
          -- Si han pasado 5 segundos desde la victoria
          if elapsed >= 5.0
             then
               -- Si quedan torneos por jugar
               if currentTournament < maxTournaments
                  then do
                    -- REINICIAR con el siguiente torneo
                    putStrLn $ "\n=== Torneo " ++ show currentTournament ++ " finalizado ==="
                    putStrLn $ "Ganador: Tanque " ++ show winId ++ " en tiempo " ++ show winTime ++ "s"
                    putStrLn $ "\n=== Iniciando Torneo " ++ show (currentTournament + 1) ++ " ==="
                    newGameState (currentTournament + 1)
                  else do
                    return gameState
             else
               -- Seguir mostrando la pantalla de victoria
               return $ gameState { gameTime = gameTime gameState + dt }
      
      -- Si no hay ganador, ejecuta el juego normal
      Nothing -> return $ updateGameLogic dt gameState


updateGameLogic :: Float -> GameState -> GameState
updateGameLogic dt gameState = 
    let
        actions = map (\t -> handleActions t (getBot gameState t)) (tanks gameState) 
        ws = worldSize gameState

        newTanks = [ reduceCooldown( updatePosition tank dt ws ) dt | (tank, _) <- actions, isRobotAlive tank ]
        newProyectiles = concat [ projectile | (_, projectile) <- actions ]
        allProyectiles = proyectiles gameState ++ newProyectiles
        updatedProyectiles = updateProyectiles allProyectiles dt

        gameState' = gameState {proyectiles = updatedProyectiles}
        movedTanksGame = gameState' {tanks = newTanks}
        
        gameAfterCollisions = checkCollisions movedTanksGame

        updatedExplosions = [ explosion | explosion <- updateExplosions (explosions gameAfterCollisions) dt, explosionTime explosion <= 2]
        updatedObstacles = updateObstacles (obstacles gameAfterCollisions) dt

        obstaclesExplosions = [ Explosion (obstaclePosition obs) 0 | obs <- updatedObstacles, obstacleTime obs < Just 0 ]

        tanksAfterExplosions = applyMineDamage (tanks gameAfterCollisions) updatedObstacles
        gameAfterExplosions = gameAfterCollisions { tanks = tanksAfterExplosions }

        newObstacles = [obs | obs <- updatedObstacles, obstacleTime obs >= Just 0]
        newUpdatedExplosions = updatedExplosions ++ obstaclesExplosions

        livingTanks = filter isRobotAlive (tanks gameAfterExplosions)
        finalTime = gameTime gameAfterExplosions + dt
        finalGame = gameAfterExplosions { gameTime = finalTime, explosions = newUpdatedExplosions, obstacles = newObstacles }

    in 
      case livingTanks of
         [winTank] -> finalGame { winner = Just (idTank winTank, finalTime) }
         _         -> finalGame


handleInputIO :: Event -> GameState -> IO GameState
handleInputIO (EventKey (SpecialKey KeySpace) Down _ _) gameState =
    case winner gameState of
        -- Solo avanzamos al siguinete torneo pulsando el espacio cuando el actual este acabado
        Just (winId, winTime) -> 
            let maxTournaments = getNumTournaments
                currentTournament = tournament gameState
            in
                if currentTournament < maxTournaments
                    then do
                        putStrLn $ "\n=== Torneo " ++ show currentTournament ++ " finalizado (saltado) ==="
                        putStrLn $ "Ganador: Tanque " ++ show winId ++ " en tiempo " ++ show winTime ++ "s"
                        putStrLn $ "\n=== Iniciando Torneo " ++ show (currentTournament + 1) ++ " ==="
                        newGameState (currentTournament + 1)
                    else return gameState
        Nothing -> return gameState
handleInputIO _ g = return g


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