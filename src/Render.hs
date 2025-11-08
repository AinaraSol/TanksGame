module Render where

import Constants
import Entities
import Tank
import Graphics.Gloss

import Data.Maybe (fromMaybe)


drawGame :: GameState -> Picture   
drawGame game = 
    let
        -- Para el HUD
        livingRobots = length (filter isRobotAlive (tanks game))
        activeProjectiles = length (proyectiles game)
        currentTournament = tournament game
        maxTournaments = getNumTournaments
        currentTime = floor (gameTime game)
        
        posX = - (fst (worldSize game)) + 10
        posY = (snd (worldSize game)) - 20
        hudPictures = [
          -- torneo actual
          translate posX posY $ scale 0.15 0.15 $ color yellow $ 
            text ("Tournament: " ++ show currentTournament ++ "/" ++ show maxTournaments),
          
          -- ponemos el tiempo del torneo
          translate posX (posY - 20) $ scale 0.15 0.15 $ color white $ 
            text ("Time: " ++ show currentTime ++ "s"),
          
          -- numero de barcos activos
          translate posX (posY - 40) $ scale 0.15 0.15 $ color white $ 
            text ("Ships alive: " ++ show livingRobots),
          
          -- numero de proyectiles
          translate posX (posY - 60) $ scale 0.15 0.15 $ color white $ 
            text ("Projectiles: " ++ show activeProjectiles)
          ]
    in
    Pictures (
        -- 1. Dibuja el fondo 
       [background game] ++

        -- 2. Dibuja obstáculos
        [ drawObstacle obstacle | obstacle <- obstacles game ] ++ 

        -- 3. Dibuja tanques VIVOS (filtro anti-zombies)
        [drawTank tank | tank <- tanks game, isRobotAlive tank] ++

        -- 4. Dibuja explosiones
        [drawExplosion explosion game | explosion <- explosions game] ++

        -- 5. Dibuja los proyectiles
        [drawProyectile projectile game | projectile <- proyectiles game] ++

        -- 6. Dibuja el HUD
        hudPictures ++

        -- 7. Dibuja el mensaje de victoria
        (drawWinner (winner game) (gameTime game) currentTournament maxTournaments)
    )

-- Función auxiliar para dibujar el mensaje de victoria
drawWinner :: Maybe (Int, Float) -> Float -> Int -> Int -> [Picture]
drawWinner Nothing _ _ _ = []
drawWinner (Just (winId, winTime)) currentTime currentT maxT =
    let
        elapsed = currentTime - winTime
        remaining = 5.0 - elapsed
        
        -- Determinar si es el último torneo
        isLastTournament = currentT >= maxT
        
        -- Texto de la cuenta atrás
        countdownText = if isLastTournament
                        then if remaining <= 0
                             then "Tournamets Finished! Close the window"
                             else "Last Tournament. Closing in " ++ show (ceiling remaining) ++ "s"
                        else if remaining <= 0
                             then "Loading next tournament..."
                             else "Next tournament in " ++ show (ceiling remaining) ++ "s (or press SPACE BAR)"
    in
    [
      -- Fondo
      color (makeColor 0 0 0 0.7) (rectangleSolid 800 250),
      
      -- Texto de torneo actual
      translate (-150) 60 $ scale 0.3 0.3 $ color yellow $
        text ("Tournament " ++ show currentT ++ "/" ++ show maxT),
      
      -- Texto principal (Centrado)
      translate (-350) 10 $ scale 0.5 0.5 $ color white $
        text ("The ship " ++ show winId ++ " won!"),
      
      -- Tiempo de victoria
      translate (-150) (-30) $ scale 0.2 0.2 $ color white $
        text ("Time: " ++ show (floor winTime) ++ " seconds"),
      
      -- Texto countdown (Pequeño, abajo y centrado)
      translate (-250) (-70) $ scale 0.2 0.2 $ color white $
        text countdownText
    ]


drawTank :: Tank -> Picture
drawTank tank = Pictures [
        -- Cuerpo del tanque
        translate (posX) (posY) $ rotate (-tankAngle) $ tankPicture tank, -- El rotate se pone en minuscula para evitar conflicto con la accion Rotate
        -- translate (posX) (posY) $ rotate (-tankAngle) $ color red $ rectangleWire tankLength tankWidth, -- Caja de colisiones para debuguear

        -- Torreta
        translate (posX) (posY) $ rotate (-turretAngle) $ translate 24 0 $ turretPicture tankTurret, -- Para la hacer la rotación de la torreta primero se translada el centro de rotación, parte gruesa de la torreta (De ahí el `translate 24 0`), al centro del dibujo y después se realiza la rotación.
        -- translate posX posY $ rotate (-turretAngle) $ pictures [
        --         color red $ rectangleSolid 5 5, --torreta
        --         translate 5 0 $ color red $ rectangleSolid 15 3 --cañon
        --     ], -- Caja de colisiones para debuguear

        -- Barra de vida
        translate (posX) (posY + 50) $ color green $ rectangleWire (0.4*100) 5,
        translate (posX - 0.2*(100-tankHealth)) (posY + 50) $ color green $ rectangleSolid (0.4*tankHealth) 5 --Esto es la barra de vida del tanque, la posicion hay que desplazarla hacia la izquierda en función de la vida del tanque
    ] 
    where
        baseObject = tankBaseObject tank
        tankAngle = orientation baseObject
        (posX, posY) = position baseObject
        tankHealth = fromIntegral (fromMaybe 0 (health tank)) -- Usamos fromMaybe 0 para que si la vida es Nothing (muerto),
        tankTurret = turret tank
        turretAngle = turretOrientation tankTurret         -- tankHealth sea 0.0 y la barra se dibuje vacía.
    

drawProyectile :: Proyectile -> GameState -> Picture
drawProyectile proyectile game = translate (posX) (posY) $ rotate (-angle) $  projectilePicture game
    where
        baseObject = proyectileBaseObject proyectile
        angle = orientation baseObject
        (posX, posY) = position baseObject

drawExplosion :: Explosion -> GameState -> Picture
drawExplosion explosion game = translate (posX) (posY) $  explosionPicture
    where
        dt = explosionTime explosion
        explosionPicturesList = explosionPictures game
        explosionIndex = min (length explosionPicturesList - 1) (floor (dt * 5))
        explosionPicture = explosionPicturesList !! explosionIndex
        (posX, posY) = explosionPosition explosion

drawObstacle :: Obstacle -> Picture
drawObstacle obstacle = Pictures [
  translate (posX) (posY + 20) $ scaledObstaclePicture
  -- translate (posX) (posY) $ color red $ circle $ fromIntegral $ damageRange obstacle -- Caja de colisiones para debuguear
  ]
    where
        dt = fromMaybe 100 $ obstacleTime obstacle
        obstaclePicturesList = obstaclePictures obstacle
        pictureIndex = (mod (floor (dt * 10)) (length obstaclePicturesList) )
        obstaclePicture = obstaclePicturesList !! pictureIndex
        scaledObstaclePicture = if obstacleClass obstacle == 2 
                               then scale 0.1 0.1 obstaclePicture 
                               else obstaclePicture
        (posX, posY) = obstaclePosition obstacle
