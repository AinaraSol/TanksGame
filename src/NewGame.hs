module NewGame where

import Entities
import Constants

import System.Random(randomRIO)

import Control.Monad(replicateM)

import Data.Maybe

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy

-- Esta función crea un nuevo GameState con numTanks tanques y una lista de proyectilis vacía
newGameState :: IO GameState
newGameState = do
    maybeBackground <- loadJuicyPNG ("assets/Background_tile.png")
    projectileMaybePicture <- loadJuicyPNG ("assets/Projectile.png")
    explosionMaybePictures <- mapM (\i -> loadJuicyPNG ("assets/Explosion/Explosion_" ++ show i ++ ".png")) [1..10]
    tankList <- mapM newTank [1..numTanks]
    obstacleList <- newObstacles
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

    return $ GameState tankList proyectileList explosionList obstacleList worldSize gameTime winner background projectilePicture explosionPictures

newObstacles :: IO [Obstacle]
newObstacles = do
    simpleObstacles <-  replicateM numSimpleObstacles (newObstacle 0)
    damageObstacles <- replicateM numDamageObstacles (newObstacle 1)
    explosionObstacles <- replicateM numExplosionObstacles (newObstacle 2)
    
    return $ simpleObstacles ++ damageObstacles ++ explosionObstacles

newObstacle :: Int -> IO Obstacle
newObstacle obstacleClass = do
    rx <- randomRIO(-size, size)
    ry <- randomRIO(-size, size)
    obstacleMaybePicture <- loadJuicyPNG ("assets/Obstacles/obstacle_" ++ show(obstacleClass) ++ ".png")
    let 
        obstaclePosition = (rx, ry)
        obstacleDamage 
            | obstacleClass == 0 = 0
            | obstacleClass == 1 = 10
            | otherwise = 30
        damageRange
            | obstacleClass == 2 = 15
            | otherwise = 0
        obstaclePicture = fromJust obstacleMaybePicture
    return $ Obstacle obstacleClass obstaclePosition obstacleDamage damageRange obstaclePicture


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