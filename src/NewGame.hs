module NewGame where

import Entities
import Constants
import Geometry
import Collisions

import System.Random(randomRIO)

import Control.Monad(replicateM)

import Data.Maybe
import qualified Data.Map as Map --para evitar conflictos de funciones con mismo nombre diferente tipo
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import Graphics.Gloss


-- Esta función crea un nuevo GameState con numTanks tanques y una lista de proyectilis vacía
newGameState :: Int -> IO GameState
newGameState t = do
    maybeBackground <- loadJuicyPNG ("assets/Background_tile.png")
    projectileMaybePicture <- loadJuicyPNG ("assets/Projectile.png")
    explosionMaybePictures <- mapM (\i -> loadJuicyPNG ("assets/Explosion/Explosion_" ++ show i ++ ".png")) [1..10]
    tankList <- mapM newTank [1..numTanks]
    obstacleList <- newObstacles
    let
        proyectileList = []
        explosionList = []
        worldSize = (getSizeX, getSizeY)
        gameTime = 0
        winner = Nothing
        tournament = t
        initialStats = Map.fromList [(idTank t, BotStats 0 0 0 False) | t <- tankList]
        statsSaved = False
        numberOfTilesX = fromIntegral $ ceiling (getSizeX / tileSize) -- la mitad de los cuadrados que caben en el ancho
        numberOfTilesY = fromIntegral $ ceiling (getSizeY / tileSize) -- la mitad de los cuadrados que caben en el alto
        
        -- Crear el fondo repitiendo la imagen del tile
        -- se calcula la posicion de cada tile en base a su índice y el tamaño del tile
        background = Pictures [translate (x * tileSize) (y * tileSize) (fromJust maybeBackground) | x <- [-numberOfTilesX .. numberOfTilesX], y <- [-numberOfTilesY .. numberOfTilesY]]
        
        projectilePicture = fromJust projectileMaybePicture
        explosionPictures = map fromJust explosionMaybePictures

    return $ GameState tankList proyectileList explosionList obstacleList worldSize gameTime winner background projectilePicture explosionPictures tournament initialStats statsSaved

newObstacles :: IO [Obstacle]
newObstacles = do
    let obstacleSpecs =
          replicate numSimpleObstacles 0 ++
          replicate numDamageObstacles 1 ++
          replicate numSwirlObstacles 2 ++
          replicate numMineObstacles 3
    generateObstaclesAvoidingOverlap obstacleSpecs []

-- | Genera una lista de obstáculos sin que se solapen entre sí.
generateObstaclesAvoidingOverlap :: [Int] -> [Obstacle] -> IO [Obstacle]
generateObstaclesAvoidingOverlap [] acc = return acc
generateObstaclesAvoidingOverlap (cls:rest) acc = do
    newObs <- generateUniqueObstacle cls acc 20 --numero de intentos para intentar generar un obstáculo que no se solape con los ya existentes
    generateObstaclesAvoidingOverlap rest (acc ++ [newObs])

-- | Genera un obstáculo aleatorio que no se solape con los existentes.
generateUniqueObstacle :: Int -> [Obstacle] -> Int -> IO Obstacle
generateUniqueObstacle _ _ 0 = error "No se pudo colocar obstáculo sin solapamiento tras muchos intentos."
generateUniqueObstacle cls existing tries = do
    candidate <- newObstacle cls
    if any (obstaclesOverlap candidate) existing
       then generateUniqueObstacle cls existing (tries - 1)
       else return candidate

-- | Determina si dos obstáculos se solapan usando su forma.
obstaclesOverlap :: Obstacle -> Obstacle -> Bool
obstaclesOverlap o1 o2 =
    any (`pointInPolygon` verts2) verts1 || any (`pointInPolygon` verts1) verts2
  where
    verts1 = obstacleVertices o1
    verts2 = obstacleVertices o2


newObstacle :: Int -> IO Obstacle
newObstacle obstacleClass = do
    rx <- randomRIO(-(getSizeX - 40), (getSizeX - 40))
    ry <- randomRIO(-(getSizeY - 40), (getSizeY - 40))
    obstacleMaybePicture <- 
        if obstacleClass == 2
            then mapM (\i -> loadJuicyPNG ("assets/Obstacles/remolino/whirl_" ++ show i ++ ".png")) [0..13]
            else mapM (\i -> loadJuicyPNG ("assets/Obstacles/obstacle_" ++ show (obstacleClass) ++ ".png")) [0]
    let 
        obstaclePosition = (rx, ry)
        obstacleDamage 
            | obstacleClass == 0 = 0
            | obstacleClass == 1 = 10
            | obstacleClass == 2 = 1
            | otherwise = 50
        damageRange
            | obstacleClass == 2 = 80
            | obstacleClass == 3 = 100
            | otherwise = 0
        obstacleTime
            | obstacleClass == 3 = Just 1
            | otherwise = Just 0
        obstacleTrigger = False
        obstaclePictures = map fromJust obstacleMaybePicture
    return $ Obstacle obstacleClass obstaclePosition obstacleDamage damageRange obstacleTime obstacleTrigger obstaclePictures


newTank :: Int -> IO Tank
newTank id = do
    tankMaybePicture <- loadJuicyPNG ("assets/Boat/Boat_" ++ show (id `mod` 4 + 1) ++ ".png")
    turretMaybePicture <- loadJuicyPNG ("assets/Boat/Cannon.png")
    rx <- randomRIO(-(getSizeX - 40), (getSizeX - 40))
    ry <- randomRIO(-(getSizeY - 40), (getSizeY - 40))
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