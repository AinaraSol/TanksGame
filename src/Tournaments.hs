module Tournaments where

import Entities
import NewGame
import Constants
import Bots
import Tank
import Collisions
import qualified Data.Map as Map
import Data.Maybe
import System.IO
import Control.Exception
import Text.Read
import Data.List
import qualified Data.Ord 
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
                    -- Guardar las estadísticas al final del torneo en estadisticas.txt
                    putStrLn $ "Guardando estadísticas del torneo " ++ show currentTournament ++ "..."
                    appendStatsToFile (formatTournamentStats gameState)
                    -- REINICIAR con el siguiente torneo
                    putStrLn $ "\n=== Torneo " ++ show currentTournament ++ " finalizado ==="
                    putStrLn $ "Ganador: Tanque " ++ show winId ++ " en tiempo " ++ show winTime ++ "s"
                    putStrLn $ "\n=== Iniciando Torneo " ++ show (currentTournament + 1) ++ " ==="
                    newGameState (currentTournament + 1)
                  else do
                    if not (statsSaved gameState)
                       then do
                         putStrLn $ "Guardando estadísticas finales del torneo " ++ show currentTournament ++ "..."
                         -- 1. Formatea las estadísticas del torneo
                         let stats = formatTournamentStats gameState
                         -- 2. Formatea el resumen
                         let summary = formatSummary (gameTime gameState)
                         -- 3. Combina y escribe una única vez para evitar error de condición de carrera si lo hicéramos por separado
                         appendStatsToFile (stats ++ summary)

                         -- Devuelve el estado con la bandera actualizada
                         return $ gameState { statsSaved = True }
                       else
                         -- Ya se guardaron, simplemente devuelve el estado sin hacer nada
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
        -- Actualiza el tiempo vivo de cada bot
        statsWithTime = updateStatsTime (currentStats gameAfterCollisions) livingTanks dt
        finalGame = gameAfterExplosions { gameTime = finalTime, explosions = newUpdatedExplosions, obstacles = newObstacles, currentStats = statsWithTime }

    in 
      case livingTanks of
         [winTank] ->
            let winnerId = idTank winTank
                -- Marca al ganador en las estadísticas
                statsWithWinner = Map.adjust (\s -> s { isWinner = True }) winnerId (currentStats finalGame)
            in finalGame { winner = Just (idTank winTank, finalTime), 
                           currentStats = statsWithWinner } 
         _         -> finalGame

updateStatsTime :: CurrentStats -> [Tank] -> Float -> CurrentStats
updateStatsTime stats livingTanks dt =
    let livingIds = map idTank livingTanks
        -- Aplica la actualización solo a los bots cuyo ID está en 'livingIds'
        updateFunc k statsData = if k `elem` livingIds
                                 then statsData { timeAlive = timeAlive statsData + dt }
                                 else statsData
    in Map.mapWithKey updateFunc stats

handleInputIO :: Event -> GameState -> IO GameState
handleInputIO (EventKey (SpecialKey KeySpace) Down _ _) gameState =
    case winner gameState of
        -- Solo avanzamos al siguiente torneo pulsando el espacio cuando el actual este acabado
        Just (winId, winTime) -> 
            let maxTournaments = getNumTournaments
                currentTournament = tournament gameState
            in
                if currentTournament < maxTournaments
                    then do
                        putStrLn $ "\n=== Guardando estadísticas Torneo " ++ show currentTournament ++ " (saltado) ==="
                        appendStatsToFile(formatTournamentStats gameState)
                        putStrLn $ "\n=== Torneo " ++ show currentTournament ++ " finalizado (saltado) ==="
                        putStrLn $ "Ganador: Tanque " ++ show winId ++ " en tiempo " ++ show winTime ++ "s"
                        putStrLn $ "\n=== Iniciando Torneo " ++ show (currentTournament + 1) ++ " ==="
                        newGameState (currentTournament + 1)
                        
                    else 
                      if not (statsSaved gameState)
                        then do
                            putStrLn $ "\n=== Guardando estadísticas finales (saltado) ==="
                            
                            let stats = formatTournamentStats gameState
                            let summary = formatSummary (gameTime gameState)
                            appendStatsToFile (stats ++ summary)

                            return $ gameState { statsSaved = True } -- Marca como guardado
                        else
                            return gameState
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


--  Estadísticas del torneo actual.
formatTournamentStats :: GameState -> String
formatTournamentStats gs = 
    let stats = currentStats gs
        tourneyNum = tournament gs
        duration = gameTime gs
        
    -- Formatea las estadísticas para este torneo
        header = "\n--- Torneo " ++ show tourneyNum ++ " (Duración: " ++ show (floor duration) ++ "s) ---\n"
        -- Ordena los bots por daño infligido (de mayor a menor)
        sortedStats = sortBy (Data.Ord.comparing (Data.Ord.Down . damageDealt . snd)) (Map.toList stats)
        
        -- Formatea cada línea de bot
        formatStat (botId, stat) =
            let percentTime = 100 * (timeAlive stat / duration)
                winnerMark = if isWinner stat then " [GANADOR]" else ""
            in "  Bot " ++ show botId ++ ":" ++ winnerMark ++
               "\n    - Daño Infligido: " ++ show (damageDealt stat) ++
               "\n    - Impactos:       " ++ show (hitsLanded stat) ++
               "\n    - Tiempo Vivo:    " ++ show (floor (timeAlive stat)) ++ "s (" ++ show (floor percentTime) ++ "%)"
               
        -- Combina todo
        outputLines = header : map formatStat sortedStats
      in
    unlines outputLines -- solo devolvemos el String
  where
    handleIOError :: SomeException -> IO ()
    handleIOError e = putStrLn $ "Error al escribir estadísticas: " ++ show e


-- | Lee el archivo de estadísticas y añade un resumen agregado.
formatSummary :: Float -> String
formatSummary totalGameTime = 
   let summaryHeader = "\n\n=== RESUMEN DE TODOS LOS TORNEOS ===\n"
       summaryBody = "  (La generación de estadísticas agregadas (medias, máximos)\n" ++
                      "   requeriría un estado global persistente entre torneos\n" ++
                      "   o un parser para leer los datos ya escritos.)\n" ++
                      "   Total de torneos jugados: " ++ show getNumTournaments ++ "\n" ++
                      "   Tiempo total de juego: " ++ show (floor totalGameTime) ++ "s\n"
    in
        summaryHeader ++ summaryBody -- Solo devolvemos el String
  
appendStatsToFile :: String -> IO ()
appendStatsToFile statsOutput = do
    appendFile "estadisticas.txt" statsOutput `catch` handleIOError
  where
    handleIOError :: SomeException -> IO ()
    handleIOError e = putStrLn $ "Error al escribir estadísticas: " ++ show e