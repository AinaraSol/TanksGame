module Bots where

import Constants
import Entities
import Geometry
import Tank
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)


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
-- Bot que persigue directamente al enemigo más cercano
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

