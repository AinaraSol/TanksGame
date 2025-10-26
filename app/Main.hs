module Main where -- Descomentar esta linea cuando se implemente la funcion main

import Bots
import Entities
import Geometry
import Tank
import Types
import Collisions
import Render

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color

import System.Random(randomRIO)

import Data.Maybe(isJust)

numTanks :: Int
numTanks = 4

size :: Float
size = 200

-- Esta función crea un nuevo GameState con numTanks tanques y una lista de proyectilis vacía
newGameState :: IO GameState
newGameState = do
    tankList <- mapM newTank [1..numTanks]
    let
        proyectileList = []
        explosionList = []
        worldSize = (size, size)
        gameTime = 0
        winner = Nothing
        
    return $ GameState tankList proyectileList explosionList worldSize gameTime winner

newTank :: Int -> IO Tank
newTank id = do
    rx <- randomRIO(-size, size)
    ry <- randomRIO(-size, size)
    let
        tankId = id
        turret = Turret 0
        health = Just 100
        memory = []
        tankBaseObject = BaseObject (rx, ry) (0, 0) 0
        cooldown = 0

    return $ Tanque tankId turret health memory tankBaseObject cooldown

main :: IO ()
main = do
    newState <- newGameState -- Con esto convertimos el IO GameState a GameState
    play
        (InWindow "Tanks Game" (round size*4,round size*4) (0,0))
        earthBrown
        60
        newState
        drawGame
        handleInput
        updateGame

earthBrown :: Color
earthBrown = makeColorI 222 184 135 255

updateGame :: Float -> GameState -> GameState
updateGame dt gameState = 
    case winner gameState of
      -- 1. Si ya hay un ganador, solo avanza el tiempo
      Just _ -> gameState { gameTime = gameTime gameState + dt }
      
      -- 2. Si no, ejecuta el juego
      Nothing -> 
        let
            --Tomamos unas actions por cada tanque
            actions = map (\t -> 
                let bot = case idTank t of
                            4 -> chaserBot gameState t
                            _ -> if odd (idTank t)
                                 then aggressiveBot gameState t
                                 else opportunistBot gameState t
                in handleActions t bot
              ) (tanks gameState)
            
            --Actualizamos las posiciones
            newTanks = [ reduceCooldown( updatePosition tank dt ) dt | (tank, _) <- actions, isRobotAlive tank ] --
            newProyectiles = concat [ projectile | (_, projectile) <- actions ]
            allProyectiles = (proyectiles gameState) ++ newProyectiles
            updatedProyectiles = updateProyectiles allProyectiles dt


            gameState' = gameState {proyectiles = updatedProyectiles}
            movedTanksGame = gameState' {tanks = newTanks}
            
            --Aplicamos las colisones
            gameAfterCollisions = checkCollisions movedTanksGame --

            updatedExplosions = [ explosion | explosion <- updateExplosions (explosions gameAfterCollisions) dt, explosionTime explosion <= 3]

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
