module Render where

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
        
        -- Dibuja el HUD en la esquina superior izquierda
        hudPictures = [
          translate (-390) 380 $ scale 0.15 0.15 $ color black $ 
            text ("Robots: " ++ show livingRobots),
          translate (-390) 360 $ scale 0.15 0.15 $ color black $ 
            text ("Proyectiles: " ++ show activeProjectiles)
          ]
    in
    Pictures (
        -- 1. Dibuja tanques VIVOS (filtro anti-zombies)
        [drawTank tank | tank <- tanks game, isRobotAlive tank] ++
        
        -- 2. Dibuja los proyectiles
        [drawProyectile projectile | projectile <- proyectiles game] ++
        
        -- 3. Dibuja explosiones
        [drawExplosion explosion | explosion <- explosions game] ++

        -- 4. Dibuja el HUD
        hudPictures ++
        
        -- 5. Dibuja el mensaje de victoria (si existe)
        (drawWinner (winner game) (gameTime game))
    )
-- Función auxiliar para dibujar el mensaje de victoria
drawWinner :: Maybe (Int, Float) -> Float -> [Picture]
drawWinner Nothing _ = [] -- Si no hay ganador, no dibujes nada
drawWinner (Just (winId, winTime)) currentTime =
    let
        elapsed = currentTime - winTime
        remaining = 5.0 - elapsed -- 5 segundos de cuenta atrás
        
        -- Texto de la cuenta atrás
        countdownText = if remaining <= 0
                        then "Lista para cerrar"
                        else "Cierre la ventana en " ++ show (ceiling remaining) ++ "s"
    in
    [
      -- Fondo
      color (makeColor 0 0 0 0.7) (rectangleSolid 800 200),
      
      -- Texto principal (Centrado)
      translate (-350) 25 $ scale 0.5 0.5 $ color white $
        text ("¡Tanque " ++ show winId ++ " ha ganado!"),
      
      -- Texto countdown (Pequeño, abajo y centrado)
      translate (-115) (-50) $ scale 0.2 0.2 $ color white $
        text countdownText
    ]


drawTank :: Tank -> Picture
drawTank tank = Scale 1.5 1.5 $ Pictures [
        translate (posX) (posY + 15) $ color green $ rectangleWire (0.4*100) 5,
        translate (posX - 0.2*(100-tankHealth)) (posY + 15) $ color green $ rectangleSolid (0.4*tankHealth) 5, --Esto es la barra de vida del tanque, la posicion hay que desplazarla hacia la izquierda en función de la vida del tanque
        translate (posX) (posY) $ rotate (-tankAngle) $ rectangleSolid 20 10, -- El rotate se pone en minuscula para evitar conflicto con la accion Rotate
        translate posX posY $ rotate (-turretAngle) $ pictures [ 
                color yellow $ rectangleSolid 5 5, --torreta
                translate 10 0 $ color yellow $ rectangleSolid 15 3 --cañon
            ]
    ] 
    where
        baseObject = tankBaseObject tank
        tankAngle = orientation baseObject
        (posX, posY) = position baseObject
        tankHealth = fromIntegral (fromMaybe 0 (health tank)) -- Usamos fromMaybe 0 para que si la vida es Nothing (muerto),
        turretAngle = turretOrientation (turret tank)         -- tankHealth sea 0.0 y la barra se dibuje vacía.
    

drawProyectile :: Proyectile -> Picture
drawProyectile proyectile = Scale 1.5 1.5 $ translate (posX) (posY) $ rotate (angle) $ circleSolid 2
    where
        baseObject = proyectileBaseObject proyectile
        angle = orientation baseObject
        (posX, posY) = position baseObject

drawExplosion :: Explosion -> Picture
drawExplosion explosion = Scale 1.5 1.5 $ translate (posX) (posY) $ Color red $ circleSolid (10*sin(dt))
    where
        dt = explosionTime explosion
        (posX, posY) = explosionPosition explosion