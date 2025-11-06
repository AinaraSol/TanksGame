module Geometry where

import Types

--        - distanceBetween :: Position -> Position -> Distance. Calcula la distancia euclidiana entre dos posiciones en el espacio. Toma dos puntos como entrada y devuelve la distancia lineal que los separa.
 
distanceBetween :: Position -> Position -> Distance
distanceBetween (x1, y1) (x2, y2) = sqrt (dx^2 + dy^2)
    where
        dx = x2 - x1
        dy = y2 - y1

--        - angleToTarget :: Position -> Position -> Angle. Determina el ángulo desde una posición origen hacia una posición objetivo. Útil para calcular la dirección en la que debe apuntar o moverse un objeto.
angleToTarget :: Position -> Position -> Angle
angleToTarget (x1, y1) (x2, y2) = rad2deg (atan2 dy dx)
    where
        dx = x2 - x1
        dy = y2 - y1

--        - deg2rad :: Angle -> Angle. Convierte un ángulo expresado en grados a su equivalente en radianes.
deg2rad :: Angle -> Angle
deg2rad degrees = degrees * pi / 180
 
 
--        - rad2deg :: Angle -> Angle. Convierte un ángulo expresado en radianes a su equivalente en grados.
rad2deg :: Angle -> Angle
rad2deg radians = radians * 180 / pi



--        - subVec :: Vector -> Vector -> Vector. Realiza la resta de dos vectores, devolviendo un 
-- nuevo vector que representa la diferencia entre ellos.

subVec :: Vector -> Vector -> Vector
subVec (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)



-- Esta función rota los puntos respecto al centro de los cuatro puntos

--getVertices :: (Point, Point, Point, Point, Angle) -> [Point]
--getVertices (p1, p2, p3, p4, angle) = 
  --  [rotatePoint c p angle | p <- pts]
  --where
    --pts = [p1, p2, p3, p4]
    --c = center pts

getVertices :: (Point, Point, Point, Point, Angle) -> [Point]
getVertices (p1, p2, p3, p4, angle) =
  -- usamos Applicative sobre listas:
  -- rotatePoint c <$> pts <*> pure angle
  -- que es equivalente a map (\p -> rotatePoint c p angle) pts
  rotated
  where
    pts = [p1, p2, p3, p4]
    c = center pts
    rotated = rotatePoint c <$> pts <*> pure angle --patrón tipico 

-- | Comprueba si un punto está dentro de un polígono (para evitar solapamientos)
pointInPolygon :: Point -> [Point] -> Bool
pointInPolygon (px, py) vertices =
    odd . length . filter id $ zipWith edgeCross vertices (tail (cycle vertices))
  where
    edgeCross (x1, y1) (x2, y2)
      | (y1 > py) /= (y2 > py) =
          let xIntersect = (x2 - x1) * (py - y1) / (y2 - y1) + x1
           in px < xIntersect
      | otherwise = False

rotatePoint :: Point -> Point -> Angle -> Point
rotatePoint (cx, cy) (x, y) angle =
    ( roundN 2 (cx + (x' * cos rad - y' * sin rad)) -- roundN 2 x redondea el numero x y lo deja con dos decimales
    , roundN 2 (cy + (x' * sin rad + y' * cos rad))
    )
  where
    x' = x - cx
    y' = y - cy
    rad = deg2rad angle

roundN :: Integer -> Float -> Float
roundN n x = fromIntegral(round (x * factor))/factor
    where factor = 10 ^ n

center :: [Point] -> Point
center pts = (avg xs, avg ys)
  where
    (xs,ys) = unzip pts --cambio
    avg zs = sum zs / fromIntegral (length zs)

--        - dot :: Point -> Point -> Float. Calcula el producto escalar (dot product) entre dos puntos tratados como vectores
dot :: Point -> Point -> Float
dot (x1,y1) (x2,y2) = x1*x2 + y1*y2


--        - sub :: Point -> Point -> Point. Resta un punto de otro, devolviendo un nuevo punto que representa la diferencia entre las coordenadas.
sub :: Point -> Point -> Point
sub (x1,y1) (x2,y2) = (x1 - x2, y1 - y2)



--        - perp :: Vector -> Vector.
-- Calcula el vector perpendicular a un punto dado (tratado como vector).

perp :: Vector -> Vector
perp (x1,x2) = (-x2, x1)


-- mul: tal que (w,h) `mul` (sw,sh) = (w * sw, h * sh)

mul::Vector -> Vector -> Vector
(mul) (w,h) (sw,sh) = (w*sw, h*sh)
