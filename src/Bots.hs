module Bots where

import Constants
import Entities
import Geometry
import Tank
import Data.List (minimumBy)
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (comparing)
import Collisions
import Memory
import Types 

import Control.Monad(join)
import Data.Foldable (find)


-- 1. 'botRegistry' actúa como un "mapa" o "diccionario" que asocia el 'Int'
--    guardado en la memoria del tanque con la función del bot real.
--
-- 2. 'getBot' es la única función que 'Main.hs' necesita conocer. Esta función
--    se encarga de leer la memoria del tanque, buscar el bot correcto en el
--    'botRegistry' y ejecutarlo.
--
-- Resultado: 'Main.hs' ya no sabe nada sobre los bots específicos. El motor
-- ('Main.hs') está "separado" de la lógica de IA ('Bots.hs').

botRegistry :: [(Int, Bot)]
botRegistry = [
    (0, aggressiveBot)
    ,(1, opportunistBot)
    ,(2, chaserBot)
    , (3, runnerBot)
  ]

getBot :: GameState -> Tank -> [Action]
getBot game self =
    let
        -- 1. Lee el Int de la memoria (por defecto 3, chaserBot)
        botId = fromMaybe 3 (join (readMemoryInt "bot" (memory self)))
        
        -- 2. Busca la función Bot en nuestra lista 'botRegistry'
        maybeBotFunc = lookup botId botRegistry
        
        -- 3. Usa el bot encontrado, o 'chaserBot' por defecto si el ID no estaba
        botFunc = fromMaybe chaserBot maybeBotFunc
        
    in
        -- 4. Ejecuta la función del bot correspondiente
        botFunc game self

-- Funciones Auxiliares
nearestEnemy :: Tank -> [Tank] -> Maybe Tank
nearestEnemy self ts =
    case filter (\t -> idTank t /= idTank self && isRobotAlive t) ts of
        [] -> Nothing
        enemies -> Just $ minimumBy compareDist enemies
  where
    compareDist a b =
        compare (distanceBetween (position (tankBaseObject self)) (position (tankBaseObject a)))
                (distanceBetween (position (tankBaseObject self)) (position (tankBaseObject b)))

-- Para estrategia 2
weakestEnemy :: Tank -> [Tank] -> Maybe Tank
weakestEnemy self ts =
    case filter (\t -> idTank t /= idTank self && isRobotAlive t) ts of
        [] -> Nothing
        enemies -> Just $ minimumBy (comparing health) enemies
        -- (comparing health) es un atajo para: \a b -> compare (health a) (health b)
  
--Bot de Ejemplo 1 (Agresivo)
aggressiveBot :: Bot
aggressiveBot game self =
  case nearestEnemy self (tanks game) of
    Nothing -> [Stay]
    Just enemy ->
      let
        myPos = position (tankBaseObject self)
        enemyPos = position (tankBaseObject enemy)

        --Leemos posiciones anteriores desde memoria
        maybeLastPos = join (readMemoryPoint "enemy_last_pos" (memory self))
        maybePrevPos = join (readMemoryPoint "enemy_pos" (memory self))

        --Calculamos velocidad estimada (si tenemos datos previos)
        enemyVel = case (maybePrevPos, maybeLastPos) of
          (Just (x2, y2), Just (x1, y1)) -> Just (x2 - x1, y2 - y1)
          _ -> Nothing

        --Estimamos posición futura
        predictionTime = 0.5
        predictedPos = case enemyVel of
          Just (vx, vy) -> (fst enemyPos + vx * predictionTime, snd enemyPos + vy * predictionTime)
          Nothing -> enemyPos  -- si no hay datos, apunta al enemigo real

        --Calculamos ángulo hacia la posición predicha
        degAngle = angleToTarget myPos predictedPos
        radAngle = deg2rad degAngle
        dist = distanceBetween myPos predictedPos
        vec = (cos radAngle * tankSpeed, sin radAngle * tankSpeed)

        --Guardamos en memoria la nueva posición
        memUpdates =
          [ UpdateMemory "enemy_last_pos" (MemPoint (Just enemyPos))
          , UpdateMemory "enemy_pos" (MemPoint (Just predictedPos))
          ]
        
        --Disparo inteligente
        actions
          | dist < 300 = [Stay, Rotate degAngle, Shoot predictedPos]
          | otherwise  = [Rotate degAngle, Move vec]
      
      in actions ++ memUpdates


-- Bot de ejemplo 2 (Oportunista)
--Este Bot va hacia el enemigo más débil
opportunistBot :: Bot
opportunistBot game self =
  -- 1. Llama a 'weakestEnemy' en lugar de 'nearestEnemy'
  fromMaybe [Stay] (actionFor <$> weakestEnemy self (tanks game))
  where
    -- 2. La lógica de 'actionFor' es perseguir al 'enemy' que le hayan pasado.
    actionFor enemy =
      let myPos    = position (tankBaseObject self)
          enemyPos = position (tankBaseObject enemy)
          dist     = distanceBetween myPos enemyPos
          degAngle = angleToTarget myPos enemyPos
          radAngle = deg2rad degAngle
          vec = (cos radAngle * tankSpeed, sin radAngle * tankSpeed)
      in if dist < 400
         then [Stay, Rotate degAngle, Shoot enemyPos]
         else [Rotate degAngle, Move vec]

--Bot de Ejemplo 3 (Chaser)
--Bot que persigue directamente al enemigo más cercano, solo colisiones
chaserBot :: Bot
chaserBot game self =
  fromMaybe [Stay] (actionFor <$> nearestEnemy self (tanks game))
  where
    actionFor enemy =
      let
          myPos    = position (tankBaseObject self)
          enemyPos = position (tankBaseObject enemy)
          degAngle = angleToTarget myPos enemyPos
          radAngle = deg2rad degAngle
          vec = (cos radAngle * tankSpeed, sin radAngle * tankSpeed)
      in [Rotate degAngle, Move vec]  -- solo gira y se mueve hacia el enemigo

--Bots de Ejemplo 4 (Cobarde)
-- Huye del enemigo más cercano.
-- Si pierde de vista al enemigo, sigue huyendo de su última posición conocida.
runnerBot :: Bot
runnerBot game self =
    let
        myPos = position (tankBaseObject self)

        -- 1. Buscar enemigo más cercano
        maybeEnemy = nearestEnemy self (tanks game)

        -- 2. Leer de memoria la última posición conocida
        maybeLastPos = join (readMemoryPoint "lastEnemy" (memory self))

        -- 3. Decidir de qué posición huir Y qué acción de memoria tomar
        (targetPos, memAction) = case maybeEnemy of
            
            -- CASO 3.1: Hay un enemigo visible
            Just enemy ->
                let pos = position (tankBaseObject enemy)
                in ( 
                     Just pos,  -- El objetivo para huir/disparar es este enemigo
                     UpdateMemory "lastEnemy" (MemPoint (Just pos)) -- Guarda su posición en memoria
                   )

            -- CASO 3.2: No hay enemigo visible
            Nothing ->
                ( 
                  maybeLastPos, -- El objetivo es la última posición guardada (si existe)
                  Stay          -- No hay nada nuevo que guardar, no hagas nada en memoria
                )

    in
        -- 4. Actuar en base al 'targetPos' que decidimos
        case targetPos of
            
            -- Si tenemos una posición (actual o de memoria) de la que huir
            Just enemyPos ->
                let
                    -- Calcular dirección de huida (vector opuesto al enemigo)
                    dirAway = sub myPos enemyPos
                    
                    -- Normalizamos la direccion
                    dirToRun = normalize dirAway
                    
                    moveVec = (fst dirToRun * tankSpeed, snd dirToRun * tankSpeed)

                    -- Apuntar la torreta al enemigo (para disparar mientras huye)
                    turretAngle = angleToTarget myPos enemyPos
                    
                    -- Disparar SÓLO si el enemigo está visible (isJust maybeEnemy)
                    -- y el cooldown está listo
                    shootActions = if isJust maybeEnemy && shootCooldown self <= 0
                                  then [Shoot enemyPos]
                                  else []
                in
                    [ Move moveVec
                    , Rotate turretAngle
                    , memAction  
                    ] ++ shootActions

            -- No hay enemigo visible Y no hay nada en memoria. Quedarse quieto.
            Nothing -> [Stay]

