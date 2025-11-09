module Statistics where

import Entities
import Text.Printf (printf)
import Data.List (maximumBy)

-- Function para borrar las estadísticas (si es necesario)
clearStatisticsFile :: IO ()
clearStatisticsFile = writeFile "estadisticas.txt" ""

-- Función para guardar las estadísticas en un archivo de texto (estadisticas.txt)
saveStatistics :: Statistics -> IO ()
saveStatistics statistics = do
    if writtenFlag statistics
      then return () -- Si ya se han escrito, no hacer nada
    else do
      let fileName = "estadisticas.txt"
      appendFile fileName (formatStatistics statistics)


-- Formatear las estadísticas para guardarlas en el archivo
formatStatistics :: Statistics -> String
formatStatistics stats =
    let header = "=== Estadísticas del Torneo " ++ show (tournamentId stats) ++ " ===\n"
        winnerStr = case winnerId stats of
                      Just wid -> "Ganador Barco ID: " ++ show wid ++ "\n"
                      Nothing  -> "No Hay Ganador\n"
        durationStr = "Duración: " ++ printf "%.2f" (duration stats) ++ " segundos\n"
        destroyedByObstaclesStr = "Tanques Destruidos por Obstáculos: " ++ show (numTanksDestroyedByObstacles stats) ++ "\n"
        tankStatsStr = concatMap formatTankStatistics (statisticsByTank stats)
    in header ++ winnerStr ++ durationStr ++ destroyedByObstaclesStr ++ "\n" ++ tankStatsStr ++ "\n"

-- Formatear las estadísticas de un tanque individual
formatTankStatistics :: TankStatistics -> String
formatTankStatistics ts =
    "\t== Barco " ++ show (tankId ts) ++ " ==\n" ++
    "\tTiempo Vivo: " ++ printf "%.2f" (timeAlive ts) ++ " segundos\n" ++
    "\tDisparos Realizados: " ++ show (numShotsFired ts) ++ "\n" ++
    "\tImpactos: " ++ show (numHits ts) ++ "\n" ++
    "\tDaño Recibido de Obstáculos: " ++ show (damageFromObstacles ts) ++ "\n\n"


saveAggregatedStatistics :: [Statistics] -> IO ()
saveAggregatedStatistics stats = do
    if writtenFlag (last stats)
      then return ()
    else do
      let fileName = "estadisticas.txt"
      let mostWinsTankId = -- calcula el ID del tanque con más victorias
            let allTankStats = concatMap statisticsByTank stats
                winCounts = [(tankId ts, length (filter (\s -> winnerId s == Just (tankId ts)) stats)) | ts <- allTankStats]
            in fst $ maximumBy (\(_, w1) (_, w2) -> compare w1 w2) winCounts
      let totalTime = sum $ map duration stats -- calcula la duración total de todos los torneos
      let totalDestroyedByObstacles = sum $ map numTanksDestroyedByObstacles stats -- total de tanques destruidos por obstáculos
      appendFile fileName $ "\n=== Estadísticas Agregadas de Torneos ===\n"
      appendFile fileName $ "Número Total de Torneos: " ++ show (length stats) ++ "\n"
      appendFile fileName $ "Barco con Más Victorias: Barco " ++ show mostWinsTankId ++ "\n"
      appendFile fileName $ "Duración Total de Todos los Torneos: " ++ printf "%.2f" totalTime ++ " segundos\n"
      appendFile fileName $ "Barcos Destruidos por Obstáculos: " ++ show totalDestroyedByObstacles ++ "\n\n"
      appendFile fileName "=== Estadísticas Agregadas por Barco ===\n\n"
      let tankIds = map tankId (statisticsByTank (head stats)) 
      let aggregatedStats = map (\tid -> aggregateTankStatistics (concatMap statisticsByTank stats) tid totalTime) tankIds -- Calcula estadísticas agregadas por cada barco
      let formattedStats = concatMap (\stats -> formatTankAggregatedStatistics stats totalTime) aggregatedStats -- Convierte a string legible las estadísticas agregadas por cada barco
      appendFile fileName formattedStats


aggregateTankStatistics :: [TankStatistics] -> Int -> Float -> TankStatistics
aggregateTankStatistics stats id totalTime =
    let filteredStats = filter (\ts -> tankId ts == id) stats
        totalShots = sum $ map numShotsFired filteredStats
        totalHits = sum $ map numHits filteredStats
        totalTimeAlive = sum $ map timeAlive filteredStats
        totalDamageFromObstacles = sum $ map damageFromObstacles filteredStats
    in TankStatistics {
        tankId = id,
        numShotsFired = totalShots,
        numHits = totalHits,
        timeAlive = totalTimeAlive,
        damageFromObstacles = totalDamageFromObstacles
    }

formatTankAggregatedStatistics :: TankStatistics -> Float -> String
formatTankAggregatedStatistics ts totalTime =
    let hitRate :: Float
        hitRate = if numShotsFired ts == 0 then 0.0 else fromIntegral (numHits ts) / fromIntegral (numShotsFired ts) * 100
        survivalRate :: Float
        survivalRate = (timeAlive ts / totalTime) * 100
    in
    "\t== Barco " ++ show (tankId ts) ++ " ==\n" ++
    "\tTiempo Vivo Total: " ++ printf "%.2f" (timeAlive ts) ++ " segundos\n" ++
    "\tDisparos Realizados Totales: " ++ show (numShotsFired ts) ++ "\n" ++
    "\tImpactos Totales: " ++ show (numHits ts) ++ "\n" ++
    "\tTasa de Impacto: " ++ printf "%.2f" hitRate ++ " %\n" ++
    "\tPorcentaje del tiempo vivo: " ++ printf "%.2f" survivalRate ++ " %\n" ++
    "\tDaño Total Recibido de Obstáculos: " ++ show (damageFromObstacles ts) ++ "\n\n"

