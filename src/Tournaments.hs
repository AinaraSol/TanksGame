module Tournaments where

import Config

runTournament :: IO ()
runTournament = do
    config <- readConfig "src/config.txt"
    let 
        numT = numTournaments config
        maxTime = maximunTime config
    putStrLn $ "Iniciando " ++ show numT ++ " torneos consecutivos..."

    forM_ [1 .. numT] $ \t -> do
        putStrLn $ "Torneo " ++ show t
        game <- newGameState t
        finalGame <- runUntilEnd game
        -- guardarEstadisticas finalGame t -- Descomentar cuando este lo de las estadisticas
        putStrLn $ "Torneo " ++ show t ++ " finalizado."
    putStrLn "Todos los torneos completados."

runUntilEnd :: GameState -> Float -> IO GameState
runUntilEnd game maxTime
  | (winner game /= Nothing) || (gameTime game >= maxTime) = return game
  | otherwise = runUntilEnd (updateGame game) -- o el paso de tiempo que uses
