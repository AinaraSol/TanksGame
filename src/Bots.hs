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
    ,(1, runnerBot)
    , (2, chaserBot)
    ,(3, trackerBot) 
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
-- Para estrategia 1
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
  

-- Modificada
-- En Bots.hs

aggressiveBot :: Bot
aggressiveBot game self =
  fromMaybe [Stay] (actionFor <$> nearestEnemy self (tanks game))
  where
    actionFor enemy =
      let myPos    = position (tankBaseObject self)
          enemyPos = position (tankBaseObject enemy)
          dist     = distanceBetween myPos enemyPos
          
          -- 1. Obtenemos el ángulo en GRADOS (para Rotate)
          degAngle = angleToTarget myPos enemyPos
          
          -- 2. Obtenemos el ángulo en RADIANES (para cos/sin)
          radAngle = deg2rad degAngle
          
          -- 3. Usamos RADIANES para calcular el vector
          vec = (cos radAngle * tankSpeed, sin radAngle * tankSpeed)
          
      in if dist < 300
         -- 4. Usamos GRADOS para la acción Rotate
         then [Stay, Rotate degAngle, Shoot enemyPos]
         -- 5. Usamos GRADOS para la acción Rotate
         else [Rotate degAngle, Move vec]


-- Bot de ejemplo 2

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
-- Bot que persigue directamente al enemigo más cercano, solo colisiones
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

--Bots de Ejemplo 4

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
            
            -- CASO A: Hay un enemigo visible
            Just enemy ->
                let pos = position (tankBaseObject enemy)
                in ( 
                     Just pos,  -- El objetivo para huir/disparar es este enemigo
                     UpdateMemory "lastEnemy" (MemPoint (Just pos)) -- Guarda su posición en memoria
                   )

            -- CASO B: No hay enemigo visible
            Nothing ->
                ( 
                  maybeLastPos, -- El objetivo es la última posición guardada (si existe)
                  Stay          -- No hay nada nuevo que guardar, no hagas nada en memoria
                )

    in
        -- 4. Actuar en base al 'targetPos' que decidimos
        case targetPos of
            
            -- Si tenemos una posición (actual o de memoria) de la que huir...
            Just enemyPos ->
                let
                    -- Calcular dirección de huida (vector opuesto al enemigo)
                    dirAway = sub myPos enemyPos
                    
                    -- (Usamos la normalización del 'circleBot' que es más segura)
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
                    , memAction  -- <--- ¡AQUÍ INCLUIMOS LA ACCIÓN DE MEMORIA!
                    ] ++ shootActions -- <--- ¡AQUÍ AÑADIMOS EL DISPARO!

            -- No hay enemigo visible Y no hay nada en memoria. Quedarse quieto.
            Nothing -> [Stay]

-- Bot de Ejemplo 5 (Tracker)
-- Fija un objetivo y lo persigue hasta que muere.
-- Reutiliza la lógica de ataque de 'opportunistBot'.
trackerBot :: Bot
trackerBot game self =
    let
        -- 1. Intenta leer el ID del objetivo de la memoria
        maybeTargetId = join (readMemoryInt "target_id" (memory self))

        -- 2. Busca ese tanque en la lista de tanques,
        --    pero solo si el ID existe Y el tanque está vivo.
        currentTarget = maybeTargetId >>= (\targetId -> 
                          find (\t -> idTank t == targetId && isRobotAlive t) (tanks game)
                        )
        
        -- 3. Define la lógica de ataque (copiada de opportunistBot)
        attackLogic enemy =
          let myPos    = position (tankBaseObject self)
              enemyPos = position (tankBaseObject enemy)
              dist     = distanceBetween myPos enemyPos
              degAngle = angleToTarget myPos enemyPos
              radAngle = deg2rad degAngle
              vec = (cos radAngle * tankSpeed, sin radAngle * tankSpeed)
              
              -- Dispara si el cooldown está listo
              shootAction = if shootCooldown self <= 0 then [Shoot enemyPos] else []
              
          in if dist < 400
             then [Stay, Rotate degAngle] ++ shootAction
             else [Rotate degAngle, Move vec] ++ shootAction

    in
        case currentTarget of
          
          -- CASO A: Tiene un objetivo válido y vivo
          -- Lo ataca usando la lógica que definimos.
          Just target -> attackLogic target

          -- CASO B: No tiene objetivo.
          -- Necesita encontrar uno nuevo.
          Nothing -> 
            case nearestEnemy self (tanks game) of
              
              -- B1: Encuentra un nuevo enemigo.
              Just newTarget -> 
                let 
                  -- Acciones de ataque
                  actions = attackLogic newTarget
                  
                  -- Acción de memoria Fija el ID del nuevo objetivo.
                  memAction = UpdateMemory "target_id" (MemInt (Just (idTank newTarget)))
                in
                  actions ++ [memAction] -- Devuelve las acciones de ataque + la de memoria

              -- B2: No quedan enemigos.
              Nothing -> 
                -- Se queda quieto y limpia la memoria.
                [Stay, UpdateMemory "target_id" (MemInt Nothing)]