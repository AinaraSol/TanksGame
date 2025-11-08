module Collisions where

import Types
import Geometry
import Entities
import Tank

import Data.Maybe (isJust)
import Data.List (find)

--TAREA 3

-- El SAT afirma:
-- Para dos polígonos convexos en el plano, si no se intersectan, entonces existe una recta (un eje) tal que las proyecciones
-- de los dos polígonos sobre esa recta no se solapan.
-- De forma equivalente, si en todas las rectas candidatas las proyecciones se solapan, entonces los polígonos sí se intersectan.
-- En 2D las “rectas candidatas” son las rectas perpendiculares a las aristas de los polígonos. 
-- Por eso basta probar los ejes normales a las aristas de ambos polígonos.
-- IDEA
-- 1º Calcular los ejes: todos los perpendiculares de A + todos los de B.
-- Para dos rectángulos, habrá 4 ejes únicos (dos horizontales y dos verticales, pero puede que algunos se repitan si están alineados).
-- Para cada eje:
-- 2º Calcular el intervalo de proyección de A y B en ese eje.
-- 3º Usar función overlap para ver si esas dos sombras se pisan.
-- Si encuentras un eje donde no se pisan →  "no hay colisión" y termina.
-- Si en todos los ejes se pisan → entonces "sí colisionan".

-- Funciones auxiliares para poder calcular SAT

--   Calcula la magnitud (longitud) de un vector (x,y).
--   Matemáticamente: |v| = sqrt(x^2 + y^2).
--   Necesario para normalizar un vector ya que debemos dividirlo por su magnitud.

magnitude :: Vector -> Float
magnitude (x,y) = sqrt (x*x + y*y)

--   Normalizar un vector (convertirlo en uno de longitud 1).
--   Si el vector es casi nulo, devolvemos (0,0) para evitar dividir entre cero.
--   Necesario porque en SAT las proyecciones deben hacerse sobre ejes unitarios,
--   así todos los cálculos están en la misma escala.
--   Ej: normalize (3,4) = (0.6,0.8)
normalize :: Vector -> Vector
normalize (x,y)
  | m <= 1e-6 = (0,0)        -- caso magnitud casi cero
  | otherwise = (x/m, y/m)   -- caso normal, convertimos el vector en unitario 
  where
    m = magnitude (x,y)


-- Proyecta un polígono (lista de vértices) sobre un eje (vector).
-- Devuelve el intervalo (mínimo, máximo) de las proyecciones.
-- Si la lista de vértices está vacía devolvemos (0,0) por seguridad.
-- Devuelve (Float , Float) y no un tipo Vector porque es un intervalo (se podria crear un tipo intervalo para mayor legibilidad con new type)
projectPolygon :: [Point] -> Vector -> (Float, Float)
projectPolygon [] _ = (0,0)
projectPolygon vertices axis =
  let unit = normalize axis                    -- normalizamos el eje
      projections = map (`dot` unit) vertices  -- proyectamos cada vértice sobre el eje normalizado
  in (minimum projections, maximum projections) -- guardamos los valores minimo, maximo en una tupla que será el intervalo de las proyecciones sobre ese eje (axis)

projectPolygon' :: [Point] -> Vector -> (Float, Float)
projectPolygon' [] _ = (0,0)
projectPolygon' vertices axis =
  let unit = normalize axis
      projections = (`dot` unit) <$> vertices   -- fmap en vez de map
  in (minimum projections, maximum projections)

--   Comprueba si dos intervalos [minA,maxA] y [minB,maxB] se solapan.
--   Necesario porque SAT dice:
--   - Si en algún eje NO se solapan → los polígonos no colisionan.
--   - Si en TODOS los ejes se solapan → sí hay colisión.
--   Ej: overlap (-1,1) (0,2) = True ; overlap (1,2) (3,4) = False
-- IDEA A = [minA,maxA] B= [minB, maxB] si maxA esta a la izq de minB no lo toca o si maxB esta a la izq de minA tampoco se tocan en el primer caso
-- A esaria a la izq de B y en el sgundo B a la izq de A
overlap :: (Float, Float) -> (Float, Float) -> Bool
overlap (minA, maxA) (minB, maxB) = not (maxA < minB || maxB < minA) --devuelve Falso cuando estan separados por al menos un eje

--   Obtiene los ejes (vectores perpendiculares a las aristas) de un polígono convexo.
--   Necesario en SAT: estos son los únicos ejes que hay que probar.
--   Se recorre cada par de vértices consecutivos (v1,v2) y se añade también (último, primero)
--   para cerrar el polígono.
--   Luego se toma la arista (v2 - v1) y se calcula su perpendicular con 'perp'.
getAxes :: [Point] -> [Vector]
getAxes []     = [] 
getAxes [_]    = []
getAxes vertices  =
  -- let pairs   = zip vertices (tail vertices) ++ [(last vertices, head vertices)] -- aristas
  let pairs = zip vertices (drop 1 (cycle vertices)) -- Lo mismo pero más eficiente

      axes = [ perp (sub v2 v1) | (v1,v2) <- pairs ]             -- lista de ejes sobre los que calcularemos cada proyeccion con projectPolygon
  in axes

getAxes' :: [Point] -> [Vector]
getAxes' []  = []
getAxes' [_] = []
getAxes' vertices =
  -- let pairs = zip vertices (tail vertices) ++ [(last vertices, head vertices)]
  let pairs = zip vertices (drop 1 (cycle vertices)) -- Hacemos lo mismo pero mas eficientemente

  in (\(v1,v2) -> perp (sub v2 v1)) <$> pairs

--Funciones Tarea 3
-- SAT booleano: devuelve True si los dos polígonos convexos colisionan
checkCollision :: [Point] -> [Point] -> Bool
checkCollision polyA polyB =
  all testAxis (getAxes polyA ++ getAxes polyB) -- aplicamos esto con todos los ejes posibles (por si un poligono esta rotado) con que un testAxis de False ya no colisionan
  where
    testAxis axis =   -- devuelve True o False dependiendo de si dos proyecciones en un mismo eje de cada poligono se pisan
      let (minA, maxA) = projectPolygon polyA axis --proyección poligono A
          (minB, maxB) = projectPolygon polyB axis --proyección Poligono B
      in overlap (minA, maxA) (minB, maxB)         -- comprobamos si se solapan o no 

-- Convertir un Proyectile en un rectángulo pequeño (o en un cuadrado) alrededor de su posición, lo mismo que hicimos con el tanque pero con el proyectile
proyectileVertices :: Proyectile -> [Point]
proyectileVertices p =
  let (cx, cy) = position (proyectileBaseObject p)
      theta = orientation (proyectileBaseObject p)
      w = 4.0
      h = 2.0
      hw = w / 2
      hh = h / 2
      rotated =getVertices ((-hw, -hh),(-hh, hw),(hw, hh),(hh, -hw),theta)
  in (\(rx, ry) -> (cx + rx, cy + ry)) <$> rotated --mismo que anterior caso

-- Devuelve los vértices del obstáculo en coordenadas globales (trasladados a obstaclePosition)
obstacleVertices :: Obstacle -> [Point]
obstacleVertices o =
  let (cx, cy) = obstaclePosition o
      w --ancho
        | obstacleClass o == 0 = 50.0  -- Sirena
        | obstacleClass o == 1 = 40.0  -- Roca
        | obstacleClass o == 2 = 30.0  -- Remolino
        | otherwise = 20.0             -- Pequeños
      h --altura
        | obstacleClass o == 0 = 75.0  -- Sirena
        | obstacleClass o == 1 = 40.0  -- Roca
        | obstacleClass o == 2 = 60.0  -- Remolino
        | otherwise = 20.0
      hw = w / 2
      hh = h / 2
      verts = [(-hw, -hh),(-hh, hw),(hw, hh),(hh, -hw)]
  in [ (cx + x, cy + y) | (x, y) <- verts ]



-- Detecta colisiones tanque-proyectil (lista de pares (Tanque, Proyectil) en colisión)
detectRobotProyectileCollisions :: [Tank] -> [Proyectile] -> [(Int, Proyectile)]
detectRobotProyectileCollisions tanks projs = -- recibe lista tanques y proyectiles
  [ (idTank t, p) | t <- tanks, p <- projs , checkCollision (tankVertices t) (proyectileVertices p) ] --con las dos funciones de antes conseguimos la lista de vertices del tanque y el proyectile rotados

-- Detecta colisiones tanque-obstaculos.
detectRobotObstacleCollisions :: [Tank] -> [Obstacle] -> [(Int, Obstacle)]
detectRobotObstacleCollisions tanks obs = -- recibe lista tanques y obstaculos
  [ (idTank t, o) | t <- tanks, o <- obs , tankCollidesObstacle t o ] 

  
--Implementacion 
detectRobotRobotCollisions :: [Tank] -> [(Int, Int)]
detectRobotRobotCollisions []     = []
detectRobotRobotCollisions (t:ts) =
  -- pares del tanque actual con todos los que quedan en la lista
  [ (idTank t, idTank t2) | t2 <- ts, checkCollision (tankVertices t) (tankVertices t2) ]
  -- más los pares que se formen recursivamente con el resto
  ++ detectRobotRobotCollisions ts


-- Función coordinadora que usa el estado del juego y devuelve uno nuevo tras aplicar los eventos de colisiones correspondientes
checkCollisions :: GameState -> GameState
checkCollisions gs =
  let ts  = tanks gs -- lista tanques vivos
      ps  = proyectiles gs -- lista proyectiles en pantalla
      es  = explosions gs
      os = obstacles gs

      -- 1. detectar colisiones
      tankProjCollisions = detectRobotProyectileCollisions ts ps
      tankTankCollisions = detectRobotRobotCollisions ts
      tankObstaclesCollisions = detectRobotObstacleCollisions ts os

      -- 2. aplicar efectos
      tsAfterProj = collisionRobotProyectileEvent' ts tankProjCollisions 
      tsAfterBoth = collisionRobotRobotEvent' tsAfterProj tankTankCollisions --la lista de tanques con vida actualizada tras contar el daño de los proyectiles
      tsAfterObstacles = collisionRobotObstacleEvent tsAfterBoth tankObstaclesCollisions

      newExplosions = generateExplosions [snd collision | collision <- tankProjCollisions]

      -- 3. eliminar proyectiles que ya colisionaron
      psAfter = removeCollidedProyectiles' ps tankProjCollisions

      -- 4. Activar las minas con las que ha habido una colision
      newObstacles = updateObstacleTriggers os tankObstaclesCollisions

  in gs { tanks = tsAfterObstacles, proyectiles = psAfter, explosions = explosions gs ++ newExplosions, obstacles = newObstacles}

-- Recibe una lista de proyectiles y una lista de colisiones (idTank,Proyectile) y devuelve la lista de proyectiles sin los proyectiles que han impactado
removeCollidedProyectiles :: [Proyectile] -> [(Int, Proyectile)] -> [Proyectile]
removeCollidedProyectiles projs collisions =
  [ p | p <- projs, p `notElem` collided ] -- lista de proyectiles sin los que han colisionado
  where
    collided = [ p | (_, p) <- collisions ] --lista proyectiles que han colisionado

removeCollidedProyectiles' :: [Proyectile] -> [(Int, Proyectile)] -> [Proyectile]
removeCollidedProyectiles' projs collisions =
  filter (`notElem` collided) projs
  where collided = snd <$> collisions   -- fmap en vez de list comprehension

-- Aplica las colisiones a la lista de tanques, restando vida
collisionRobotProyectileEvent :: [Tank] -> [(Int, Proyectile)] -> [Tank]
collisionRobotProyectileEvent tanks collisions =
  map (applyPCollision'' collisions) tanks

collisionRobotProyectileEvent' :: [Tank] -> [(Int, Proyectile)] -> [Tank]
collisionRobotProyectileEvent' tanks collisions = 
  applyPCollision'' collisions <$> tanks

collisionRobotObstacleEvent :: [Tank] -> [(Int, Obstacle)] -> [Tank]
collisionRobotObstacleEvent tanks collisions = 
  handleTankObstacle collisions <$> tanks


generateExplosions :: [Proyectile] -> [Explosion]
generateExplosions = map newExplosion

newExplosion :: Proyectile -> Explosion
newExplosion p = 
  let
    pos = position (proyectileBaseObject p)
  in Explosion pos 0

-- Aplica las colisiones a un tanque en particular
--applyPCollision :: [(Int, Proyectile)] -> Tank -> Tank
--applyPCollision collisions t =
  --let hits = [ p | (tankid, p) <- collisions, tankid == idTank t ] --guardamos en una lista los proyectiles que impactan en t
      --totalDamage = sum (map damage hits) --calcula el daño total de esos proyectiles
  --in if null hits 
       --then t
      --else t { health = health t - totalDamage } --en caso de no estar vacía le resta la vida correspondiente a t

--applyPCollision' :: [(Int, Proyectile)] -> Tank -> Tank
--applyPCollision' collisions t =
  --let hits = snd <$> filter (\(tankid, _) -> tankid == idTank t) collisions
    --  totalDamage = sum (damage <$> hits)
  --in if totalDamage == 0
   --    then t
     --  else t { health = health t - totalDamage }

applyPCollision'' :: [(Int, Proyectile)] -> Tank -> Tank
applyPCollision'' collisions t =
  let hits = snd <$> filter (\(tankid, _) -> tankid == idTank t) collisions
      totalDamage = sum (damage <$> hits)
  in if totalDamage == 0
       then t
       -- MODIFICADO:
       else t { health = reduceHealth totalDamage (health t) }

applyMineDamage :: [Tank] -> [Obstacle] -> [Tank]
applyMineDamage tanksList obstacles = map applyExplosionDamage tanksList
  where
    explosions = filter isExploding obstacles

    isExploding o = obstacleTime o <= Just 0

    applyExplosionDamage tank = foldl applyDamage tank damageSources
      where
        damageSources = [obstacleDamage o | o <- explosions, isInRange o tank]

    isInRange obstacle tank =
      distanceBetween (position (tankBaseObject tank)) (obstaclePosition obstacle)
      <= fromIntegral (damageRange obstacle)

-- Idem para las PCollisions robot robot
collisionRobotRobotEvent :: [Tank] -> [(Int, Int)] -> [Tank]
collisionRobotRobotEvent tanks collisions =
  map (applyRCollision' collisions) tanks

collisionRobotRobotEvent' :: [Tank] -> [(Int, Int)] -> [Tank]
collisionRobotRobotEvent' tanks collisions = applyRCollision' collisions <$> tanks


-- Aplica las RCollisions a un tanque 
--applyRCollision :: [(Int, Int)] -> Tank -> Tank
--applyRCollision collisions t =
  --let hits = [ 5 | (id1, id2) <- collisions, id1 == idTank t || id2 == idTank t ] -- añadimos 20 por cada choque con otro tanque
    --  totalDamage = sum hits 
  --in if null hits
   --    then t
     --  else t { health = health t - totalDamage, tankBaseObject = (tankBaseObject t) {velocity= (0,0) }}

applyRCollision' :: [(Int, Int)] -> Tank -> Tank
applyRCollision' collisions t =
  let hits = [ 5 | (id1, id2) <- collisions, id1 == idTank t || id2 == idTank t ]
      totalDamage = sum hits 
  in if null hits
       then t
       -- MODIFICADO:
       else t { health = reduceHealth totalDamage (health t), tankBaseObject = (tankBaseObject t) {velocity= (0,0) }}

-- reduceHealth: Quita vida y comprueba si el tanque muere (devuelve Nothing)
reduceHealth :: Int -> Maybe Int -> Maybe Int
reduceHealth _ Nothing = Nothing -- Si ya está muerto, sigue muerto
reduceHealth dmg (Just h) = 
    let newHealth = h - dmg
    in if newHealth <= 0
       then Nothing       -- Murió
       else Just newHealth  -- Sigue vivo con menos vida

-- Detecta y maneja colisiones tanque-obstáculo
handleTankObstacle :: [(Int, Obstacle)] -> Tank -> Tank
handleTankObstacle collisions tank =
  case find ((== idTank tank) . fst) collisions of
    Just collision -> handleTankObstaclesCollision collision tank
    Nothing        -> tank  -- si no hay colisión, devolvemos el tanque sin cambios

handleTankObstaclesCollision :: (Int,Obstacle) -> Tank -> Tank
handleTankObstaclesCollision collision tank =
  case obstacleClass (snd collision) of
        0 -> handleTankObstacleBlocking tank (snd collision)
        1 -> handleTankObstacleDamage tank (snd collision)
        2 -> handleTankObstacleSwirl tank (snd collision)
        _ -> handleTankObstacleMine tank (snd collision)


-- Obstaculo que bloquean el paso
handleTankObstacleBlocking :: Tank -> Obstacle -> Tank
handleTankObstacleBlocking tank obstacle =
  if tankCollidesObstacle tank obstacle
    then let bo = tankBaseObject tank
             (x, y) = position bo
             (vx, vy) = velocity bo
             -- retrocede un pequeño paso para salir del obstáculo
             backtrack = (x - vx * 2, y - vy * 2)
         in tank { tankBaseObject = bo { position = backtrack, velocity = (-vx,-vy) } }
    else tank


--Obstáculos que causan daño inmediato
handleTankObstacleDamage :: Tank -> Obstacle -> Tank
handleTankObstacleDamage t o =
  if tankCollidesObstacle t o --Añadir cambio de direccion del barco
    then t { health = reduceHealth (obstacleDamage o) (health t), tankBaseObject = (tankBaseObject t) {velocity= (-vx,-vy)}}
    else t
      where (vx,vy) = velocity (tankBaseObject t) 

handleTankObstacleSwirl :: Tank -> Obstacle -> Tank
handleTankObstacleSwirl t o = t {health = reduceHealth (obstacleDamage o) (health t)}

handleTankObstacleMine :: Tank -> Obstacle -> Tank
handleTankObstacleMine t o
  | obstacleTime o < Just 0 = t {health = reduceHealth (obstacleDamage o) (health t)}
  | otherwise = t 

-- Detecta si el tanque y el obstáculo están lo bastante cerca
tankCollidesObstacle :: Tank -> Obstacle -> Bool
tankCollidesObstacle tank obstacle = 
  checkCollision (tankVertices tank) (obstacleVertices obstacle) ||
  distanceBetween (position $ tankBaseObject tank) (obstaclePosition obstacle)
        < fromIntegral (damageRange obstacle)

updateObstacleTriggers :: [Obstacle] -> [(Int, Obstacle)] -> [Obstacle]
updateObstacleTriggers obstacles collisions =
  [ if any (\(_, o) -> o == obstacle && obstacleClass o == 3) collisions
      then obstacle { obstacleTrigger = True }
      else obstacle
  | obstacle <- obstacles
  ]