module Tank where

import Geometry
import Entities
import Types

import Data.Maybe (isJust)

-- detectedAgent: Determinar si un agente ha detectado a otro en caso de encontrarse dentro del rango de su radar

detectedAgent :: Tank -> Tank -> Bool
detectedAgent tank1 tank2 = distanceBetween (position (tankBaseObject tank1)) (position (tankBaseObject tank2)) <= radarRange
  where
    radarRange = 100.0  -- ejemplo de rango de radar, ajustar según sea necesario

-- isRobotAlive: True si la energía del robot es mayor a 0

--isRobotAlive::Tank -> Bool
--isRobotAlive t = health t > 0

isRobotAlive::Tank -> Bool
isRobotAlive t = isJust (health t) -- <-- MODIFICADO (antes era 'health t > 0')

-- countActiveRobots: Contar los robots que están vivos SIN MODIFICAR

--countActiveRobots :: [Tank] -> Int
--countActiveRobots tanks = length [t | t <- tanks, isRobotAlive t]

-- countActiveRobots: Contar los robots que están vivos MODIFICADA. Usa evaluación perezosa con la lista ts y fmap (id es función identidad se queda con los True)
countActiveRobots :: [Tank] -> Int 
countActiveRobots = length . filter id . fmap isRobotAlive 

--mismo caso pero con applicative (explicar diferencia). fmap f xs ≡ pure f <*> xs
countActiveRobots' :: [Tank] -> Int
countActiveRobots' = length . filter id . (pure isRobotAlive <*>) --con pure envolvemos la función isRobotAlive en una lista y tenemos que usar <*> para trabajar con el segundo argumento que está envuelto en el mismo contexto
-- pure infiere el tipo según el contexto: ej en el caso de arriba es lista porque el compilador de haskell sabe que el segundo argumento esta envuelto en el tipo lista pero si fuera un maybe tank pure envuelve la función en un Maybe

-- updateRobotVelocity: Actualiza la velocidad de un robot con una velocidad dada
updateRobotVelocity:: Tank -> Velocity -> Tank
updateRobotVelocity tank v = tank { tankBaseObject = (tankBaseObject tank) { velocity = v }}

-- updateVelocity: Actualizar velocidad basada en la acción de movimiento indicada por el bot

updateVelocity :: Action -> Tank -> Tank
updateVelocity a tank = tank { tankBaseObject = (tankBaseObject tank) { velocity = newVel }}
  where
    vel = velocity (tankBaseObject tank)
    newVel = case a of
        Move v  -> if isTankInBounds tank (200, 200) then vel `mul` v else (0, 0)
        Rotate angle -> vel
        Shoot position -> vel
        Stay -> (0,0)

-- updatePosition: Actualizar una posición en función de la velocidad y el incremento de tiempo

updatePosition :: Tank -> Float -> Tank
updatePosition tank deltaT
  | vx == 0 && vy == 0 = tank  -- si no se mueve, la posición no cambia
  | otherwise = tank { tankBaseObject = (tankBaseObject tank) { position = (x + vx * deltaT, y + vy * deltaT) } }
  where
    (x,y)   = position (tankBaseObject tank)
    (vx,vy) = velocity (tankBaseObject tank)

handleAction :: Tank -> Action -> (Tank, [Proyectile])
handleAction t a = 
  case a of
    Move vector -> (t { tankBaseObject = (tankBaseObject t) { velocity = vector} }, [])
    Shoot position -> if shootCooldown t <= 0 then
        let
         (newTank, ps) = shootPostion t position
        in ( newTank { shootCooldown = 3}, ps) --3 segundos entre cada disparo
      else
        (t, [])
    Rotate angle -> (t { tankBaseObject = (tankBaseObject t) { orientation = angle } }, [])
    Stay -> (t { tankBaseObject = (tankBaseObject t) { velocity = (0, 0) } }, [])

shootPostion :: Tank -> Position -> (Tank, [Proyectile])
shootPostion t pos =
  let
    tankPosition = position (tankBaseObject t)
    angleToPosition = angleToTarget tankPosition pos
    radAngle = deg2rad angleToPosition
    newTank = t { turret = (turret t) {turretOrientation = angleToPosition} }
    vel = (cos radAngle * 20, sin radAngle * 20)
    --Para calcular la posicion del proyectil tenemos que ponerlo fuera del tanque, para que no cause una colision con el propio tanque
    proyectilPos = (fst tankPosition + cos radAngle * 15, snd tankPosition + sin radAngle * 15)
    proyectile = Proyectile tankPosition 30 (BaseObject proyectilPos vel radAngle)
  in (newTank, [proyectile])


handleActions :: Tank -> [Action] -> (Tank, [Proyectile])
handleActions t actions = foldl 
          (\(tank, bullets) action ->
            let 
              (tank', newBullets) = handleAction tank action
            in (tank', bullets ++ newBullets)
          ) (t, []) actions

updateProyectiles :: [Proyectile] -> Float -> [Proyectile]
updateProyectiles ps dt = 
    [p { proyectileBaseObject = (proyectileBaseObject p) { position = (x + vx * dt, y + vy * dt) } } | p <- ps,
     let (x, y) = position (proyectileBaseObject p),
     let (vx, vy) = velocity (proyectileBaseObject p)
    ]

-- Convertir un Tank en sus 4 vértices (rectángulo rotado)
-- Ajusta tankPosition con orientation y un tamaño dado (podemos cambiarlo según worldSize)
tankVertices :: Tank -> [Point]
tankVertices t =
  let (cx, cy) = position (tankBaseObject t)      -- toma la posición del tanque
      theta = orientation (tankBaseObject t)             -- ángulo del tanque
      w = 20.0      -- ancho total, por ej 20
      h = 10.0      -- alto total, por ej 10
      hw = w / 2    -- para que tenga el tanque el ancho 10 unidades desde su posición a la izq y 10 hacia la derecha
      hh = h / 2    -- lo mismo pero con la altura
      rotated = getVertices ((-hw,-hh),( hw,-hh),( hw, hh),(-hw, hh),theta) -- calculamos la lista de vértices con la rotación hecha
  in map (\(rx,ry) -> (cx + rx, cy + ry)) rotated --asi quedaria alrededor del centro del tanque 

tankVertices' :: Tank -> [Point]
tankVertices' t =
  let (cx, cy) = position (tankBaseObject t)
      theta = orientation (tankBaseObject t)
      w = 20.0; h = 10.0
      hw = w / 2; hh = h / 2
      rotated = getVertices ((-hw,-hh),( hw,-hh),( hw, hh),(-hw, hh), theta)
  in (\(rx, ry) -> (cx + rx, cy + ry)) <$> rotated   -- <$> en vez de map para envolver la función lambda

--        - isInBounds :: Point -> Size -> Bool. Verifica si un punto se encuentra dentro de los límites definidos por un tamaño dado.

isInBounds :: Point -> Size -> Bool
isInBounds (x,y) (width, height)
    | x >= 0 && x <= width && y >= 0 && y <= height = True
    | otherwise = False

isTankInBounds :: Tank -> Size -> Bool
isTankInBounds tank size = all (\vertice -> isInBounds vertice size) (tankVertices tank)