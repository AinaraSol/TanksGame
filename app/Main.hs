module Main where -- Descomentar esta linea cuando se implemente la funcion main

import Bots
import Constants
import Entities
import Geometry
import Tank
import Types
import Collisions
import Render
import Memory

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Juicy


import System.Random(randomRIO)

import Data.Maybe
import Control.Monad (join) --Este join es importarlo para sacar los valores de Maybe(maybe Int)

numTanks :: Int
numTanks = 8

size :: Float
size = 400

tileSize :: Float
tileSize = 64 -- Tamaño de cada cuadrado del fondo en píxeles



-- Esta función crea un nuevo GameState con numTanks tanques y una lista de proyectilis vacía
newGameState :: IO GameState
newGameState = do
    maybeBackground <- loadJuicyPNG ("assets/Background_tile.png")
    projectileMaybePicture <- loadJuicyPNG ("assets/Projectile.png")
    explosionMaybePictures <- mapM (\i -> loadJuicyPNG ("assets/Explosion/Explosion_" ++ show i ++ ".png")) [1..10]
    tankList <- mapM newTank [1..numTanks]
    let
        proyectileList = []
        explosionList = []
        worldSize = (size, size)
        gameTime = 0
        winner = Nothing

        numberOfTilesX = fromIntegral $ ceiling (size / tileSize) -- la mitad de los cuadrados que caben en el ancho
        numberOfTilesY = fromIntegral $ ceiling (size / tileSize) -- la mitad de los cuadrados que caben en el alto
        
        -- Crear el fondo repitiendo la imagen del tile
        -- se calcula la posicion de cada tile en base a su índice y el tamaño del tile
        background = Pictures [translate (x * tileSize) (y * tileSize) (fromJust maybeBackground) | x <- [-numberOfTilesX .. numberOfTilesX], y <- [-numberOfTilesY .. numberOfTilesY]]
        
        projectilePicture = fromJust projectileMaybePicture
        explosionPictures = map fromJust explosionMaybePictures

    return $ GameState tankList proyectileList explosionList worldSize gameTime winner background projectilePicture explosionPictures

newTank :: Int -> IO Tank
newTank id = do
    tankMaybePicture <- loadJuicyPNG ("assets/Boat/Boat_" ++ show (id `mod` 4 + 1) ++ ".png")
    turretMaybePicture <- loadJuicyPNG ("assets/Boat/Cannon.png")
    rx <- randomRIO(-size, size)
    ry <- randomRIO(-size, size)
    botAsigna <- randomRIO(0,3)
    let
        tankId = id
        tankPicture = fromJust tankMaybePicture
        turretPicture = fromJust turretMaybePicture
        turret = Turret 0 turretPicture -- La torreta empieza apuntando hacia la derecha (0 grados)
        health = Just 100
        memory = [("bot",MemInt(Just botAsigna))]
        tankBaseObject = BaseObject (rx, ry) (0, 0) 0
        cooldown = 0

    return $ Tanque tankId turret health memory tankBaseObject cooldown tankPicture

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
            --Tomamos unas actions por cada tanque
            actions = map (\t -> 
                let bot = case fromMaybe 3 (join(readMemoryInt "bot" (memory t))) of
                            0 -> aggressiveBot gameState t -- Elegimos Bot aggressive segun el parametro de la memoria
                            1 -> opportunistBot gameState t -- Elegimos Bot opportunist segun el parametro de la memoria
                            2 -> circleBot gameState t -- Elegimos Bot que se mueve en circulo segun el parametro de la memoria
                            _ -> chaserBot gameState t -- Elegimos Bot chaserBot segun el parametro de la memoria
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
