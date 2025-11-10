module Tournaments where

import Entities
import NewGame
import Constants
import Bots
import Tank
import Collisions
import Statistics

import Graphics.Gloss.Interface.IO.Game

runTournament :: Float -> GameState -> IO GameState
runTournament dt gameState = 
    case winner gameState of
      -- Si ya hay un ganador
      Just (winId, winTime) -> 
        let elapsed = gameTime gameState - winTime
            maxTournaments = getNumTournaments
            currentTournament = tournament gameState
            stats = (statistics gameState) { winnerId = Just winId, duration = winTime } -- Actualizar estadísticas con el ganador y duración
        in
          -- Si han pasado 5 segundos desde la victoria
          if elapsed >= 5.0
            then do
              -- Guardar estadísticas de la partida
              saveStatistics stats -- Guardar las estadísticas en estadisticas.txt

               -- Si quedan torneos por jugar
              if currentTournament < maxTournaments
                then do
                  -- REINICIAR con el siguiente torneo
                  putStrLn $ "\n=== Torneo " ++ show currentTournament ++ " finalizado ==="
                  if winId == 99 
                    then do
                      putStrLn $ "Empate en tiempo " ++ show winTime ++ "s"
                    else do
                      putStrLn $ "Ganador: Tanque " ++ show winId ++ " en tiempo " ++ show winTime ++ "s"
                  putStrLn $ "\n=== Iniciando Torneo " ++ show (currentTournament + 1) ++ " ==="
                  newGameState (currentTournament + 1) (totalStatistics gameState ++ [stats])
                else do
                  let newTotalStats = totalStatistics gameState ++ [stats]
                  saveAggregatedStatistics newTotalStats
                  -- Marcar estadísticas como escritas
                  return gameState 
                    { statistics = stats { writtenFlag = True }
                    , totalStatistics = map (\s -> s { writtenFlag = True }) newTotalStats 
                    }
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

        updatedStatsWithShots = updateStatisticsShots (statistics gameAfterExplosions) newProyectiles
        updatedStats = updateStatisticsTimeAlive updatedStatsWithShots dt (tanks gameState) -- Usamos los tanques antes de actualizar para contar el tiempo de vida correctamente

        livingTanks = filter isRobotAlive (tanks gameAfterExplosions)
        finalTime = gameTime gameAfterExplosions + dt
        finalGame = gameAfterExplosions { gameTime = finalTime, explosions = newUpdatedExplosions, obstacles = newObstacles, statistics = updatedStats }

    in 
      case livingTanks of
         [winTank] -> finalGame { winner = Just (idTank winTank, finalTime) }
         [] | finalTime >= getMaximumTime -> finalGame { winner = Just (99, finalTime) } -- En caso de que haya un empate see pone el numero 99
         _ | finalTime >= getMaximumTime -> finalGame { winner = Just (idTank (livingTanks !! 0), finalTime) }
         _         -> finalGame


handleInputIO :: Event -> GameState -> IO GameState
handleInputIO (EventKey (SpecialKey KeySpace) Down _ _) gameState =
    case winner gameState of
        -- Solo avanzamos al siguinete torneo pulsando el espacio cuando el actual este acabado
        Just (winId, winTime) -> 
            let maxTournaments = getNumTournaments
                currentTournament = tournament gameState
                stats = (statistics gameState) { winnerId = Just winId, duration = winTime } -- Actualizar estadísticas con el ganador y duración
            in
                saveStatistics stats >>  -- Guardar estadísticas de la partida
                if currentTournament < maxTournaments
                    then do
                        putStrLn $ "\n=== Torneo " ++ show currentTournament ++ " finalizado (saltado) ==="
                        putStrLn $ "Ganador: Tanque " ++ show winId ++ " en tiempo " ++ show winTime ++ "s"
                        putStrLn $ "\n=== Iniciando Torneo " ++ show (currentTournament + 1) ++ " ==="
                        newGameState (currentTournament + 1) (totalStatistics gameState ++ [stats])
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

updateStatisticsShots :: Statistics -> [Proyectile] -> Statistics
updateStatisticsShots stats projectiles = 
    let
        tankIds = map tankIdOwner projectiles
        updatedTankStats = map (updateTankShots tankIds) (statisticsByTank stats)
    in
        stats { statisticsByTank = updatedTankStats }
  where
    updateTankShots :: [Int] -> TankStatistics -> TankStatistics
    updateTankShots tankIds ts =
        let shotsFired = length $ filter (== tankId ts) tankIds
        in ts { numShotsFired = numShotsFired ts + shotsFired }


updateStatisticsTimeAlive :: Statistics -> Float -> [Tank] -> Statistics
updateStatisticsTimeAlive stats dt tanksList = 
    let
        tankIdsAlive = map idTank $ filter isRobotAlive tanksList
        updatedTankStats = map (updateTankTimeAlive tankIdsAlive) (statisticsByTank stats)
    in
        stats { statisticsByTank = updatedTankStats }
  where
    updateTankTimeAlive :: [Int] -> TankStatistics -> TankStatistics
    updateTankTimeAlive tankIdsAlive ts =
        if tankId ts `elem` tankIdsAlive
            then ts { timeAlive = timeAlive ts + dt }
            else ts